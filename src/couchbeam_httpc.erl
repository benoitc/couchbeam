%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam_httpc).

-export([request/5,
         db_request/5, db_request/6,
         json_body/1,
         db_resp/2,
         make_headers/4,
         maybe_oauth_header/4]).
%% urls utils
-export([server_url/1, db_url/1, doc_url/2]).
%% atts utols
-export([reply_att/1, wait_mp_doc/2, len_doc_to_mp_stream/3, send_mp_doc/5]).

-include("couchbeam.hrl").

request(Method, Url, Headers, Body, Options) ->
    {FinalHeaders, FinalOpts} = make_headers(Method, Url, Headers,
                                             Options),

    hackney:request(Method, Url , FinalHeaders, Body, FinalOpts).

db_request(Method, Url, Headers, Body, Options) ->
    db_request(Method, Url, Headers, Body, Options, []).

db_request(Method, Url, Headers, Body, Options, Expect) ->
    Resp = request(Method, Url, Headers, Body, Options),
    db_resp(Resp, Expect).

json_body(Ref) ->
    {ok, Body} = hackney:body(Ref),
    couchbeam_ejson:decode(Body).

make_headers(Method, Url, Headers, Options) ->
    Headers1 = case couchbeam_util:get_value(<<"Accept">>, Headers) of
        undefined ->
            [{<<"Accept">>, <<"application/json, */*;q=0.9">>} | Headers];
        _ ->
            Headers
    end,
   {Headers2, Options1} = maybe_oauth_header(Method, Url, Headers1, Options),
   maybe_proxyauth_header(Headers2, Options1).


maybe_oauth_header(Method, Url, Headers, Options) ->
    case couchbeam_util:get_value(oauth, Options) of
        undefined ->
            {Headers, Options};
        OauthProps ->
            Hdr = couchbeam_util:oauth_header(Url, Method, OauthProps),
            {[Hdr|Headers], proplists:delete(oauth, Options)}
    end.

maybe_proxyauth_header(Headers, Options) ->
  case couchbeam_util:get_value(proxyauth, Options) of
    undefined ->
      {Headers, Options};
    ProxyauthProps ->
      {lists:append([ProxyauthProps,Headers]), proplists:delete(proxyauth, Options)}
  end.

db_resp({ok, Ref}=Resp, _Expect) when is_reference(Ref) ->
    Resp;
db_resp({ok, 401, _}, _Expect) ->
    {error, unauthenticated};
db_resp({ok, 403, _}, _Expect) ->
    {error, forbidden};
db_resp({ok, 404, _}, _Expect) ->
    {error, not_found};
db_resp({ok, 409, _}, _Expect) ->
    {error, conflict};
db_resp({ok, 412, _}, _Expect) ->
    {error, precondition_failed};
db_resp({ok, _, _}=Resp, []) ->
    Resp;
db_resp({ok, Status, Headers}=Resp, Expect) ->
    case lists:member(Status, Expect) of
        true -> Resp;
        false ->
            {error, {bad_response, {Status, Headers, <<>>}}}
    end;
db_resp({ok, 401, _, Ref}, _Expect) ->
    hackney:skip_body(Ref),
    {error, unauthenticated};
db_resp({ok, 403, _, Ref}, _Expect) ->
    hackney:skip_body(Ref),
    {error, forbidden};
db_resp({ok, 404, _, Ref}, _Expect) ->
    hackney:skip_body(Ref),
    {error, not_found};
db_resp({ok, 409, _, Ref}, _Expect) ->
    hackney:skip_body(Ref),
    {error, conflict};
db_resp({ok, 412, _, Ref}, _Expect) ->
    hackney:skip_body(Ref),
    {error, precondition_failed};
db_resp({ok, _, _, _}=Resp, []) ->
    Resp;
db_resp({ok, Status, Headers, Ref}=Resp, Expect) ->
    case lists:member(Status, Expect) of
        true -> Resp;
        false ->
            {ok, Body} = hackney:body(Ref),
            {error, {bad_response, {Status, Headers, Body}}}
    end;
db_resp(Error, _Expect) ->
    Error.


%% @doc Asemble the server URL for the given client
%% @spec server_url({Host, Port}) -> iolist()
server_url(#server{url=Url}) ->
    Url.

db_url(#db{name=DbName}) ->
    DbName.

doc_url(Db, DocId) ->
    iolist_to_binary([db_url(Db), <<"/">>, DocId]).


%% attachments handling

%% @hidden
reply_att(ok) ->
    ok;
reply_att(done) ->
    done;
reply_att({ok, 404, _, Ref}) ->
    hackney:skip_body(Ref),
    {error, not_found};
reply_att({ok, 409, _, Ref}) ->
    hackney:skip_body(Ref),
    {error, conflict};
reply_att({ok, Status, _, Ref}) when Status =:= 200 orelse Status =:= 201 ->
  {[{<<"ok">>, true}|R]} = couchbeam_httpc:json_body(Ref),
  {ok, {R}};
reply_att({ok, Status, Headers, Ref}) ->
    {ok, Body} = hackney:body(Ref),
    {error, {bad_response, {Status, Headers, Body}}};
reply_att(Error) ->
    Error.

%% @hidden
wait_mp_doc(Ref, Buffer) ->
    %% we are always waiting for the full doc
    case hackney:stream_multipart(Ref) of
        {headers, _} ->
            wait_mp_doc(Ref, Buffer);
        {body, Data} ->
            NBuffer = << Buffer/binary, Data/binary >>,
            wait_mp_doc(Ref, NBuffer);
        end_of_part when Buffer =:= <<>> ->
            %% end of part in multipart/mixed
            wait_mp_doc(Ref, Buffer);
        end_of_part ->
            %% decode the doc
            {Props} = Doc = couchbeam_ejson:decode(Buffer),
            case couchbeam_util:get_value(<<"_attachments">>, Props, {[]}) of
                {[]} ->
                    %% not attachments wait for the eof or the next doc
                    NState = {Ref, fun() -> wait_mp_doc(Ref, <<>>) end},
                    {doc, Doc, NState};
                {Atts} ->
                    %% start to receive the attachments
                    %% we get the list of attnames for the versions of
                    %% couchdb that don't provide the att name in the
                    %% header.
                    AttNames = [AttName || {AttName, _} <- Atts],
                    NState = {Ref, fun() ->
                                    wait_mp_att(Ref, {nil, AttNames})
                            end},
                    {doc, Doc, NState}
            end;
        mp_mixed ->
            %% we are starting a multipart/mixed (probably on revs)
            %% continue
            wait_mp_doc(Ref, Buffer);
        mp_mixed_eof ->
            %% end of multipar/mixed wait for the next doc
            wait_mp_doc(Ref, Buffer);
        eof ->
            eof
    end.

%% @hidden
wait_mp_att(Ref, {AttName, AttNames}) ->
    case hackney:stream_multipart(Ref) of
        {headers, Headers} ->
            %% old couchdb api doesn't give the content-disposition
            %% header so we have to use the list of att names given in
            %% the doc. Hopefully the erlang parser keeps it in order.
            case hackney_headers:get_value(<<"content-disposition">>,
                                           hackney_headers:new(Headers)) of
                undefined ->
                    [Name | Rest] = AttNames,
                    NState = {Ref, fun() ->
                                    wait_mp_att(Ref, {Name, Rest})
                            end},
                    {att, Name, NState};
                CDisp ->
                    {_, Props} = content_disposition(CDisp),
                    Name = proplists:get_value(<<"filename">>, Props),
                    [_ | Rest] = AttNames,
                    NState = {Ref, fun() ->
                                    wait_mp_att(Ref, {Name, Rest})
                            end},
                    {att, Name, NState}
            end;
        {body, Data} ->
            %% return the attachment par
            NState = {Ref, fun() -> wait_mp_att(Ref, {AttName, AttNames}) end},
            {att_body, AttName, Data, NState};
        end_of_part ->
            %% wait for the next attachment
            NState = {Ref, fun() -> wait_mp_att(Ref, {nil, AttNames}) end},
            {att_eof, AttName, NState};
        mp_mixed_eof ->
            %% wait for the next doc
            wait_mp_doc(Ref, <<>>);
        eof ->
            %% we are done with the multipart request
            eof
    end.

%% @hidden
content_disposition(Data) ->
    hackney_bstr:token_ci(Data, fun
            (_Rest, <<>>) ->
                {error, badarg};
            (Rest, Disposition) ->
                hackney_bstr:params(Rest, fun
                        (<<>>, Params) -> {Disposition, Params};
                        (_Rest2, _) -> {error, badarg}
                    end)
        end).

%% @hidden
len_doc_to_mp_stream(Atts, Boundary, {Props}) ->
    {AttsSize, Stubs} = lists:foldl(fun(Att, {AccSize, AccAtts}) ->
                    {AttLen, Name, Type, Encoding, _Msg} = att_info(Att),
                    AccSize1 = AccSize +
                               4 + %% \r\n\r\n
                               AttLen +
                               byte_size(hackney_bstr:to_binary(AttLen)) +
                               4 +  %% "\r\n--"
                               byte_size(Boundary) +
                               byte_size(Name) +
                               byte_size(Type) +
                               byte_size(<<"\r\nContent-Disposition: attachment; filename=\"\"">> ) +
                               byte_size(<<"\r\nContent-Type: ">>) +
                               byte_size(<<"\r\nContent-Length: ">>) +
                               case Encoding of
                                   <<"identity">> ->
                                       0;
                                   _ ->
                                       byte_size(Encoding) +
                                       byte_size(<<"\r\nContent-Encoding: ">>)
                               end,
                    AccAtts1 = [{Name, {[{<<"content_type">>, Type},
                                         {<<"length">>, AttLen},
                                         {<<"follows">>, true},
                                         {<<"encoding">>, Encoding}]}}
                                | AccAtts],
                    {AccSize1, AccAtts1}
            end, {0, []}, Atts),

    Doc1 = case couchbeam_util:get_value(<<"_attachments">>, Props) of
        undefined ->
            {Props ++ [{<<"_attachments">>, {Stubs}}]};
        {OldAtts} ->
            %% remove updated attachments from the old list of
            %% attachments
            OldAtts1 = lists:foldl(fun({Name, AttProps}, Acc) ->
                            case couchbeam_util:get_value(Name, Stubs) of
                                undefined ->
                                    [{Name, AttProps} | Acc];
                                _ ->
                                    Acc
                            end
                    end, [], OldAtts),
            %% update the list of the attachnebts with the attachments
            %% that will be sent in the multipart
            FinalAtts = lists:reverse(OldAtts1) ++ Stubs,
            {lists:keyreplace(<<"_attachments">>, 1, Props,
                             {<<"_attachments">>, {FinalAtts}})}
    end,

    %% eencode the doc
    JsonDoc = couchbeam_ejson:encode(Doc1),

    %% calculate the final size with the doc part
    FinalSize = 2 + % "--"
                byte_size(Boundary) +
                36 + % "\r\ncontent-type: application/json\r\n\r\n"
                byte_size(JsonDoc) +
                4 + % "\r\n--"
                byte_size(Boundary) +
                + AttsSize +
                2, % "--"

    {FinalSize, JsonDoc, Doc1}.

%% @hidden
send_mp_doc(Atts, Ref, Boundary, JsonDoc, Doc) ->
    %% send the doc
    DocParts = [<<"--", Boundary/binary >>,
                <<"Content-Type: application/json">>,
                <<>>, JsonDoc, <<>>],
    DocBin = hackney_bstr:join(DocParts, <<"\r\n">>),
    case hackney:send_body(Ref, DocBin) of
        ok ->
            send_mp_doc_atts(Atts, Ref, Doc, Boundary);
        Error ->
            Error
    end.

%% @hidden
send_mp_doc_atts([], Ref, Doc, Boundary) ->
    %% no more attachments, send the final boundary (eof)
    case hackney:send_body(Ref, <<"\r\n--", Boundary/binary, "--" >>) of
        ok ->
            %% collect the response.
            mp_doc_reply(Ref, Doc);
        Error ->
            Error
    end;

send_mp_doc_atts([Att | Rest], Ref, Doc, Boundary) ->
    {AttLen, Name, Type, Encoding, Msg} = att_info(Att),
    BinAttLen = hackney_bstr:to_binary(AttLen),
    AttHeadersParts = [<<"--", Boundary/binary >>,
                       <<"Content-Disposition: attachment; filename=\"", Name/binary, "\"" >>,
                      <<"Content-Type: ", Type/binary >>,
                      <<"Content-Length: ", BinAttLen/binary >>],

    AttHeadersParts1 = AttHeadersParts ++
            case Encoding of
                <<"identity">> ->
                    [<<>>, <<>>];
                _ ->
                    [<<"Content-Encoding: ", Encoding/binary >>, <<>>,
                     <<>>]
            end,
    AttHeadersBin = hackney_bstr:join(AttHeadersParts1, <<"\r\n">>),

    %% first send the att headers
    case hackney:send_body(Ref, AttHeadersBin) of
        ok ->
            %% send the attachment by itself
            case hackney:send_body(Ref, Msg) of
                ok ->
                    %% everything is OK continue to the next attachment
                    send_mp_doc_atts(Rest, Ref, Doc, Boundary);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @hidden
mp_doc_reply(Ref, Doc) ->
    Resp = hackney:start_response(Ref),
    case couchbeam_httpc:db_resp(Resp, [200, 201]) of
        {ok, _, _, Ref} ->
            {JsonProp} = couchbeam_httpc:json_body(Ref),
            NewRev = couchbeam_util:get_value(<<"rev">>, JsonProp),
            NewDocId = couchbeam_util:get_value(<<"id">>, JsonProp),
            %% set the new doc ID
            Doc1 = couchbeam_doc:set_value(<<"_id">>, NewDocId, Doc),
            %% set the new rev
            FinalDoc = couchbeam_doc:set_value(<<"_rev">>, NewRev, Doc1),
            %% return the updated document
            {ok, FinalDoc};
        Error ->
            Error
    end.

%% @hidden
att_info({Name, {file, Path}=Msg}) ->
    CType = mimerl:filename(hackney_bstr:to_binary(Name)),
    Len = filelib:file_size(Path),
    {Len, Name, CType, <<"identity">>, Msg};
att_info({Name, Bin}) when is_list(Bin) ->
    att_info({Name, iolist_to_binary(Bin)});
att_info({Name, Bin}) when is_binary(Bin) ->
    CType = mimerl:filename(hackney_bstr:to_binary(Name)),
    {byte_size(Bin), Name, CType, <<"identity">>, Bin};
att_info({Name, {file, Path}=Msg, Encoding}) ->
    CType = mimerl:filename(hackney_bstr:to_binary(Name)),
    Len = filelib:file_size(Path),
    {Len, Name, CType, Encoding, Msg};
att_info({Name, {Fun, _Acc0}=Msg, Len}) when is_function(Fun) ->
    {Len, Name, <<"application/octet-stream">>, <<"identity">>, Msg};
att_info({Name, Fun, Len}) when is_function(Fun) ->
    {Len, Name, <<"application/octet-stream">>, <<"identity">>, Fun};
att_info({Name, Bin, Encoding}) when is_binary(Bin) ->
    CType = mimerl:filename(hackney_bstr:to_binary(Name)),
    {byte_size(Bin), Name, CType, Encoding, Bin};
att_info({Name, {Fun, _Acc0}=Msg, Len, Encoding}) when is_function(Fun) ->
    {Len, Name, <<"application/octet-stream">>, Encoding, Msg};
att_info({Name, Fun, Len, Encoding}) when is_function(Fun) ->
    {Len, Name, <<"application/octet-stream">>, Encoding, Fun};
att_info({Name, Bin, CType, Encoding}) when is_binary(Bin) ->
    {byte_size(Bin), Name, CType, Encoding, Bin};
att_info({Name, {Fun, _Acc0}=Msg, Len, CType, Encoding})
                when is_function(Fun) ->
    {Len, Name, CType, Encoding, Msg};
att_info({Name, Fun, Len, CType, Encoding}) when is_function(Fun) ->
    {Len, Name, CType, Encoding, Fun}.
