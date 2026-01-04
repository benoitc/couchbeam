%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam_view).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-include("couchbeam.hrl").

-export([stream/2, stream/3,
         cancel_stream/1, stream_next/1,
         fetch/1, fetch/2, fetch/3,
         count/1, count/2, count/3,
         first/1, first/2, first/3,
         all/1, all/2,
         fold/4, fold/5,
         foreach/3, foreach/4,
         parse_view_options/1,
         show/2, show/3, show/4
        ]).

-define(COLLECT_TIMEOUT, 10000).

-spec all(Db::db()) ->
          {ok, Rows::list(ejson_object())} |
          {error, term()} |
          {error, term(), Rows::list(ejson_object())}.
%% @doc fetch all docs
%% @equiv fetch(Db, 'all_docs', [])
all(Db) ->
    fetch(Db, 'all_docs', []).

-spec all(Db::db(), Options::view_options()) ->
          {ok, Rows::list(ejson_object())} |
          {error, term()} |
          {error, term(), Rows::list(ejson_object())}.
%% @doc fetch all docs
%% @equiv fetch(Db, 'all_docs', Options)
all(Db, Options) ->
    fetch(Db, 'all_docs', Options).

-spec fetch(Db::db()) ->
          {ok, Rows::list(ejson_object())} |
          {error, term()} |
          {error, term(), Rows::list(ejson_object())}.
%% @equiv fetch(Db, 'all_docs', [])
fetch(Db) ->
    fetch(Db, 'all_docs', []).

-spec fetch(Db::db(), ViewName::'all_docs' | {DesignName::design_name(),
                                              ViewName::view_name()})
           -> {ok, Rows::list(ejson_object())} | {error, term()}.
%% @equiv fetch(Db, ViewName, [])
fetch(Db, ViewName) ->
    fetch(Db, ViewName,[]).


-spec fetch(Db::db(), ViewName::'all_docs' | {DesignName::design_name(),
                                              ViewName::view_name()}, Options::view_options())
           -> {ok, Rows::list(ejson_object())} | {error, term()}.
%% @doc Collect view results
%%  <p>Db: a db record</p>
%%  <p>ViewName: <code>'all_docs'</code> to get all docs or <code>{DesignName,
%%  ViewName}</code></p>
%%  <pre>Options :: view_options() [{key, binary()}
%%    | {start_docid, binary()} | {startkey_docid, binary()}
%%    | {end_docid, binary()} | {endkey_docid, binary()}
%%    | {start_key, binary()} | {end_key, binary()}
%%    | {limit, integer()}
%%    | {stale, stale()}
%%    | descending
%%    | {skip, integer()}
%%    | group | {group_level, integer()}
%%    | {inclusive_end, boolean()} | {reduce, boolean()} | reduce | include_docs | conflicts
%%    | {keys, list(binary())}
%%    | async_query</pre>
%% <p>See {@link couchbeam_view:stream/4} for more information about
%% options.</p>
%% <p>Return: {ok, Rows} or {error, Error}</p>
fetch(Db, ViewName, Options) ->
    case proplists:is_defined(sync_query, Options) of
        true -> fetch_sync(Db, ViewName, Options);
        false -> fetch_async(Db, ViewName, Options)
    end.

fetch_async(Db, ViewName, Options) ->
    case stream(Db, ViewName, Options) of
        {ok, Ref} ->
            Timeout = proplists:get_value(collect_timeout, Options, ?COLLECT_TIMEOUT),
            collect_view_results(Ref, [], Timeout);
        Error ->
            Error
    end.

fetch_sync(Db, ViewName, Options) ->
    make_view(Db, ViewName, Options, fetch_sync_fun(Db)).

fetch_sync_fun(Db) ->
    fun(Args, Url) ->
        case view_request(Db, Url, Args) of
            {ok, _, _, Ref} ->
                {Props} = couchbeam_httpc:json_body(Ref),
                {ok, couchbeam_util:get_value(<<"rows">>, Props)};
            Error ->
                Error
        end
    end.

-spec show(db(), {binary(), binary()}) ->
          {'ok', ejson_object()} |
          {'error', term()}.
show(Db, ShowName) ->
    show(Db, ShowName, <<>>).

-spec show(db(), {binary(), binary()}, binary()) ->
          {'ok', ejson_object()} |
          {'error', term()}.
show(Db, ShowName, DocId) ->
    show(Db, ShowName, DocId, []).

-type show_option() :: {'query_string', binary()}. % "foo=bar&baz=biz"
-type show_options() :: [show_option()].

-spec show(db(), {binary(), binary()}, 'null' | binary(), show_options()) ->
          {'ok', ejson_object()} |
          {'error', term()}.
show(#db{server=Server, options=DBOptions}=Db
    ,{<<DesignName/binary>>, <<ShowName/binary>>}
    ,DocId
    ,Options
    ) ->
    URL = hackney_url:make_url(couchbeam_httpc:server_url(Server)
                              ,iolist_to_binary([couchbeam_httpc:db_url(Db)
                                                ,<<"/_design/">>
                                                ,DesignName
                                                ,<<"/_show/">>
                                                ,ShowName
                                                ,show_doc_id(DocId)
                                                ,get_query_string(Options)
                                                ]
                                               )
                              ,Options
                              ),

    case couchbeam_httpc:db_request('get', URL, [], <<>>, DBOptions, [200]) of
        {ok, _, _, Ref} ->
            {'ok', couchbeam_httpc:json_body(Ref)};
        Error -> Error
    end.

get_query_string(Options) ->
    case proplists:get_value('query_string', Options) of
        'undefined' -> <<>>;
        <<>> -> <<>>;
        <<KVs/binary>> -> <<"?", KVs/binary>>
    end.

show_doc_id('null') -> <<>>;
show_doc_id(<<>>) -> <<>>;
show_doc_id(<<DocId/binary>>) -> [<<"/">>, couchbeam_util:encode_docid(DocId)].

-spec stream(Db::db(), ViewName::'all_docs' | {DesignName::design_name(),
                                               ViewName::view_name()}) -> {ok, StartRef::term(),
                                                                           ViewPid::pid()} | {error, term()}.
%% @equiv stream(Db, ViewName, Client, [])
stream(Db, ViewName) ->
    stream(Db, ViewName, []).

-spec stream(Db::db(), ViewName::'all_docs' | {DesignName::design_name(),
                                               ViewName::view_name()}, Options::view_options())
            -> {ok, StartRef::term()} | {error, term()}.
%% @doc stream view results to a pid
%%  <p>Db: a db record</p>
%%  <p>ViewName: 'all_docs' to get all docs or {DesignName,
%%  ViewName}</p>
%%  <p>Client: pid where to send view events where events are:
%%  <dl>
%%      <dt>{row, StartRef, done}</dt>
%%          <dd>All view results have been fetched</dd>
%%      <dt>{row, StartRef, Row :: ejson_object()}</dt>
%%          <dd>A row in the view</dd>
%%      <dt>{error, StartRef, Error}</dt>
%%          happend.</dd>
%%  </dl></p>
%%  <p><pre>Options :: view_options() [{key, binary()}
%%    | descending
%%    | {skip, integer()}
%%    | group | {group_level, integer()}
%%    | {inclusive_end, boolean()} | {reduce, boolean()} | reduce | include_docs | conflicts
%%    | {keys, list(binary())}
%%    | `{stream_to, Pid}': the pid where the changes will be sent,
%%      by default the current pid. Used for continuous and longpoll
%%      connections</pre>
%%
%%  <ul>
%%      <li><code>{key, Key}</code>: key value</li>
%%      <li><code>{start_docid, DocId}</code> | <code>{startkey_docid, DocId}</code>: document id to start with (to allow pagination
%%          for duplicate start keys</li>
%%      <li><code>{end_docid, DocId}</code> | <code>{endkey_docid, DocId}</code>: last document id to include in the result (to
%%          allow pagination for duplicate endkeys)</li>
%%      <li><code>{start_key, Key}</code>: start result from key value</li>
%%      <li><code>{end_key, Key}</code>: end result from key value</li>
%%      <li><code>{limit, Limit}</code>: Limit the number of documents in the result</li>
%%      <li><code>{stale, Stale}</code>: If stale=ok is set, CouchDB will not refresh the view
%%      even if it is stale, the benefit is a an improved query latency. If
%%      stale=update_after is set, CouchDB will update the view after the stale
%%      result is returned. If stale=false is set, CouchDB will update the view before
%%      the query. The default value of this parameter is update_after.</li>
%%      <li><code>descending</code>: reverse the result</li>
%%      <li><code>{skip, N}</code>: skip n number of documents</li>
%%      <li><code>group</code>: the reduce function reduces to a single result
%%      row.</li>
%%      <li><code>{group_level, Level}</code>: the reduce function reduces to a set
%%      of distinct keys.</li>
%%      <li><code>{reduce, boolean()}</code>: whether to use the reduce function of the view. It defaults to
%%      true, if a reduce function is defined and to false otherwise.</li>
%%      <li><code>include_docs</code>: automatically fetch and include the document
%%      which emitted each view entry</li>
%%      <li><code>{inclusive_end, boolean()}</code>: Controls whether the endkey is included in
%%      the result. It defaults to true.</li>
%%      <li><code>conflicts</code>: include conflicts</li>
%%      <li><code>{keys, [Keys]}</code>: to pass multiple keys to the view query</li>
%%  </ul></p>
%%
%% <p> Return <code>{ok, StartRef, ViewPid}</code> or <code>{error,
                                                %Error}</code>. Ref can be
%% used to disctint all changes from this pid. ViewPid is the pid of
%% the view loop process. Can be used to monitor it or kill it
%% when needed.</p>
stream(Db, ViewName, Options) ->
    {To, Options1} = case proplists:get_value(stream_to, Options) of
                         undefined ->
                             {self(), Options};
                         Pid ->
                             {Pid, proplists:delete(stream_to, Options)}
                     end,
    make_view(Db, ViewName, Options1, fun(Args, Url) ->
                                              Ref = make_ref(),
                                              Req = {Db, Url, Args},
                                              case supervisor:start_child(couchbeam_view_sup, [To,
                                                                                               Ref,
                                                                                               Req,
                                                                                               Options]) of
                                                  {ok, _Pid} ->
                                                      {ok, Ref};
                                                  Error ->
                                                      Error
                                              end
                                      end).


cancel_stream(Ref) ->
    with_view_stream(Ref, fun(Pid) ->
                                  case supervisor:terminate_child(couchbeam_view_sup, Pid) of
                                      ok ->
                                          case supervisor:delete_child(couchbeam_view_sup,
                                                                       Pid) of
                                              ok ->
                                                  ok;
                                              {error, not_found} ->
                                                  ok;
                                              Error ->
                                                  Error
                                          end;
                                      Error ->
                                          Error
                                  end
                          end).

stream_next(Ref) ->
    with_view_stream(Ref, fun(Pid) ->
                                  Pid ! {Ref, stream_next}
                          end).

-spec count(Db::db()) -> integer() | {error, term()}.
%% @equiv count(Db, 'all_docs', [])
count(Db) ->
    count(Db, 'all_docs', []).

-spec count(Db::db(), ViewName::'all_docs' | {DesignName::design_name(),
                                              ViewName::view_name()}) -> integer() | {error, term()}.
%% @equiv count(Db, ViewName, [])
count(Db, ViewName) ->
    count(Db, ViewName, []).

-spec count(Db::db(), ViewName::'all_docs' | {DesignName::design_name(),
                                              ViewName::view_name()}, Options::view_options())
           -> integer() | {error, term()}.
%% @doc count number of doc in a view (or all docs)
count(Db, ViewName, Options)->
    %% make sure we set the limit to 0 here so we don't have to get all
    %% the results to count them...
    Options1 = couchbeam_util:force_param(limit, 0, Options),

    %% make the request
    make_view(Db, ViewName, Options1, fun(Args, Url) ->
                                              case view_request(Db, Url, Args) of
                                                  {ok, _, _, Ref} ->
                                                      {Props} = couchbeam_httpc:json_body(Ref),
                                                      couchbeam_util:get_value(<<"total_rows">>, Props);
                                                  Error ->
                                                      Error
                                              end
                                      end).

-spec first(Db::db()) -> {ok, Row::ejson_object()} | {error, term()}.
first(Db) ->
    first(Db, 'all_docs', []).

-spec first(Db::db(), ViewName::'all_docs' | {DesignName::design_name(),
                                              ViewName::view_name()})
           -> {ok, Row::ejson_object()} | {error, term()}.
%% @equiv first(Db, ViewName, [])
first(Db, ViewName) ->
    first(Db, ViewName,[]).


-spec first(Db::db(), ViewName::'all_docs' | {DesignName::design_name(),
                                              ViewName::view_name()}, Options::view_options())
           -> {ok, Rows::ejson_object()} | {error, term()}.
%% @doc get first result of a view
%%  <p>Db: a db record</p>
%%  <p>ViewName: 'all_docs' to get all docs or {DesignName,
%%  ViewName}</p>
%%  <pre>Options :: view_options() [{key, binary()}
%%    | {start_docid, binary()} | {startkey_docid, binary()}
%%    | {end_docid, binary()} | {endkey_docid, binary()}
%%    | {start_key, binary()} | {end_key, binary()}
%%    | {limit, integer()}
%%    | {stale, stale()}
%%    | descending
%%    | {skip, integer()}
%%    | group | {group_level, integer()}
%%    | {inclusive_end, boolean()} | {reduce, boolean()} | reduce | include_docs | conflicts
%%    | {keys, list(binary())}</pre>
%% <p>See {@link couchbeam_view:stream/4} for more information about
%% options.</p>
%% <p>Return: {ok, Row} or {error, Error}</p>
first(Db, ViewName, Options) ->
    %% we only want 1 result so force the limit to 1. no need to fetch
    %% all the results
    Options1 = couchbeam_util:force_param(limit, 1, Options),

    %% make the request
    make_view(Db, ViewName, Options1, fun(Args, Url) ->
                                              case view_request(Db, Url, Args) of
                                                  {ok, _, _, Ref} ->
                                                      {Props} = couchbeam_httpc:json_body(Ref),
                                                      case couchbeam_util:get_value(<<"rows">>, Props) of
                                                          [] ->
                                                              {ok, nil};
                                                          [Row] ->
                                                              {ok, Row}
                                                      end;
                                                  Error ->
                                                      Error
                                              end
                                      end).

-spec fold(Function::function(), Acc::any(), Db::db(),
           ViewName::'all_docs' | {DesignName::design_name(), ViewName::view_name()})
          -> list(term()) | {error, term()}.
%% @equiv fold(Function, Acc, Db, ViewName, [])
fold(Function, Acc, Db, ViewName) ->
    fold(Function, Acc, Db, ViewName, []).

-spec fold(Function::function(), Acc::any(), Db::db(),
           ViewName::'all_docs' | {DesignName::design_name(),
                                   ViewName::view_name()}, Options::view_options())
          -> list(term()) | {error, term()}.
%% @doc call Function(Row, AccIn) on succesive row, starting with
%% AccIn == Acc. Function/2 must return a new list accumultator or the
%% atom <em>done</em> to stop fetching results. Acc0 is returned if the
%% list is empty. For example:
%% ```
%% couchbeam_view:fold(fun(Row, Acc) -> [Row|Acc] end, [], Db, 'all_docs').
%% '''
fold(Function, Acc, Db, ViewName, Options) ->
    %% make sure we stream item by item so we can stop at any time.
    Options1 = couchbeam_util:force_param(async, once, Options),
    %% start iterrating the view results
    case stream(Db, ViewName, Options1) of
        {ok, Ref} ->
            fold_view_results(Ref, Function, Acc);
        Error ->
            Error
    end.

-spec foreach(Function::function(), Db::db(),
              ViewName::'all_docs' | {DesignName::design_name(), ViewName::view_name()})
             -> list(term()) | {error, term()}.
%% @equiv foreach(Function, Db, ViewName, [])
foreach(Function, Db, ViewName) ->
    foreach(Function, Db, ViewName, []).

-spec foreach(Function::function(),  Db::db(),
              ViewName::'all_docs' | {DesignName::design_name(),
                                      ViewName::view_name()}, Options::view_options())
             -> list(term()) | {error, term()}.
%% @doc call Function(Row) on succesive row. Example:
%% ```
%% couchbeam_view:foreach(fun(Row) -> io:format("got row ~p~n", [Row]) end, Db, 'all_docs').
%% '''
foreach(Function, Db, ViewName, Options) ->
    FunWrapper = fun(Row, _Acc) ->
                         Function(Row),
                         ok
                 end,
    fold(FunWrapper, ok, Db, ViewName, Options).


%% ----------------------------------
%% utilities functions
%% ----------------------------------

-spec parse_view_options(Options::list()) -> view_query_args().
%% @doc parse view options
parse_view_options(Options) ->
    parse_view_options(Options, #view_query_args{}).

parse_view_options([], Args) ->
    Args;
parse_view_options([{key, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{key, couchbeam_ejson:encode(Value)}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{start_docid, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{startkey_docid, Value}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{startkey_docid, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{startkey_docid, Value}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{end_docid, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{end_docid, Value}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{endkey_docid, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{endkey_docid, Value}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{start_key, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{start_key, couchbeam_ejson:encode(Value)}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{end_key, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{end_key, couchbeam_ejson:encode(Value)}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{startkey, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{startkey, couchbeam_ejson:encode(Value)}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{endkey, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{endkey, couchbeam_ejson:encode(Value)}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{limit, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{limit, Value}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{stale, ok}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{stale, "ok"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{stale, update_after}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{stale, "update_after"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{stale, false}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{stale, "false"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{stale, _}|_Rest], _Args) ->
    {error, "invalid stale value"};
parse_view_options([descending|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{descending, "true"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([group|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{group, "true"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{group_level, Level}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{group_level, Level}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([inclusive_end|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{inclusive_end, "true"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{inclusive_end, true}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{inclusive_end, "true"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{inclusive_end, false}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{inclusive_end, "false"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([reduce|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{reduce, "true"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{reduce, true}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{reduce, "true"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{reduce, false}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{reduce, "false"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([include_docs|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{include_docs, "true"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([conflicts|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{conflicts, "true"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{skip, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{skip, Value}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{list, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{list, Value}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{keys, Value}|Rest], Args) ->
    parse_view_options(Rest, Args#view_query_args{method=post,
                                                  keys=Value});
parse_view_options([{Key, Value}|Rest], #view_query_args{options=Opts}=Args)
  when is_list(Key) ->
    Opts1 = [{Key, Value}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([_|Rest], Args) ->
    parse_view_options(Rest, Args).

%% @private

make_view(#db{server=Server}=Db, ViewName, Options, Fun) ->
    Args = parse_view_options(Options),
    ListName = proplists:get_value(list, Options),
    case ViewName of
        'all_docs' ->
            Url = hackney_url:make_url(couchbeam_httpc:server_url(Server),
                                       [couchbeam_httpc:db_url(Db), <<"_all_docs">>],
                                       Args#view_query_args.options),
            Fun(Args, Url);
        {DName, VName} when ListName =:= 'undefined' ->
            Url = hackney_url:make_url(couchbeam_httpc:server_url(Server),
                                       [couchbeam_httpc:db_url(Db), <<"_design">>,
                                        DName, <<"_view">>, VName],
                                       Args#view_query_args.options),
            Fun(Args, Url);
        {DName, VName} ->
            Url = hackney_url:make_url(couchbeam_httpc:server_url(Server),
                                       [couchbeam_httpc:db_url(Db), <<"_design">>,
                                        DName, <<"_list">>, ListName, VName],
                                       Args#view_query_args.options),
            Fun(Args, Url);
        _ ->
            {error, invalid_view_name}
    end.

fold_view_results(Ref, Fun, Acc) ->
    receive
        {Ref, done} ->
            Acc;
        {Ref, {row, Row}} ->
            case Fun(Row, Acc) of
                stop ->
                    cancel_stream(Ref),
                    Acc;
                Acc1 ->
                    stream_next(Ref),
                    fold_view_results(Ref, Fun, Acc1)
            end;
        {Ref, Error} ->
            {error, Acc, Error}
    end.

-spec collect_view_results(reference(), Rows::list(ejson_object()), Timeout::timeout()) ->
          {ok, Rows::list(ejson_object())} |
          {error, term()} |
          {error, term(), Rows::list(ejson_object())}.
collect_view_results(Ref, Acc, Timeout) ->
    receive
        {Ref, done} ->
            Rows = lists:reverse(Acc),
            {ok, Rows};
        {Ref, {row, Row}} ->
            collect_view_results(Ref, [Row|Acc], Timeout);
        {Ref, {error, Error}}
          when Acc =:= []->
            {error, Error};
        {Ref, {error, Error}} ->
            %% in case we got some results
            Rows = lists:reverse(Acc),
            {error, Error, Rows}
    after Timeout ->
            {error, timeout}
    end.

view_request(#db{options=Opts}, Url, Args) ->
    case Args#view_query_args.method of
        get ->
            couchbeam_httpc:db_request(get, Url, [], <<>>,
                                       Opts, [200]);
        post ->
            Body = couchbeam_ejson:encode(
                     {[{<<"keys">>, Args#view_query_args.keys}]}
                    ),

            Hdrs = [{<<"Content-Type">>, <<"application/json">>}],
            couchbeam_httpc:db_request(post, Url, Hdrs, Body,
                                       Opts, [200])
    end.

with_view_stream(Ref, Fun) ->
    case ets:lookup(couchbeam_view_streams, Ref) of
        [] ->
            {error, stream_undefined};
        [{Ref, Pid}] ->
            %% Check if the process is still alive to avoid race conditions
            case is_process_alive(Pid) of
                true ->
                    Fun(Pid);
                false ->
                    %% Clean up the stale entry
                    ets:delete(couchbeam_view_streams, Ref),
                    {error, stream_undefined}
            end
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Mock view tests - return pre-computed results based on JS map function logic
%% Uses sync_query option to avoid async streaming which is harder to mock
view_test() ->
    couchbeam_mocks:setup(),
    try
        {ok, _} = application:ensure_all_started(couchbeam),

        %% Mock db_request for view queries
        meck:expect(couchbeam_httpc, db_request,
            fun(get, Url, _H, _B, _O, _E) ->
                view_mock_response(Url);
               (post, Url, _H, _B, _O, _E) ->
                view_mock_response(Url)
            end),

        Server = couchbeam:server_connection(),
        Db = #db{server=Server, name = <<"couchbeam_testdb">>, options=[]},

        %% Test _all_docs - returns 3 docs (design doc + 2 test docs)
        {ok, AllDocs} = couchbeam_view:fetch(Db, 'all_docs', [sync_query]),
        ?assertEqual(3, length(AllDocs)),

        %% Test named view - returns docs where type == "test" (2 docs)
        {ok, Rst} = couchbeam_view:fetch(Db, {"couchbeam", "test"}, [sync_query]),
        ?assertEqual(2, length(Rst)),

        %% Test count
        Count = couchbeam_view:count(Db, {"couchbeam", "test"}),
        ?assertEqual(2, Count),

        %% Test first
        {ok, {FirstRow}} = couchbeam_view:first(Db, {"couchbeam", "test"}, [include_docs, sync_query]),
        {Doc1} = proplists:get_value(<<"doc">>, FirstRow),
        ?assertEqual(<<"test">>, proplists:get_value(<<"type">>, Doc1)),

        %% Test with start_key/end_key
        {ok, Rst3} = couchbeam_view:fetch(Db, {"couchbeam", "test"}, [{start_key, <<"test">>}, sync_query]),
        ?assertEqual(4, length(Rst3)),

        {ok, Rst4} = couchbeam_view:fetch(Db, {"couchbeam", "test"}, [{start_key, <<"test">>}, {end_key, <<"test3">>}, sync_query]),
        ?assertEqual(3, length(Rst4)),
        ok
    after
        couchbeam_mocks:teardown()
    end.

view_notfound_test() ->
    couchbeam_mocks:setup(),
    try
        {ok, _} = application:ensure_all_started(couchbeam),

        %% Mock db_request to return 404 for non-existent view
        meck:expect(couchbeam_httpc, db_request,
            fun(get, _Url, _H, _B, _O, _E) ->
                {error, not_found}
            end),

        Server = couchbeam:server_connection(),
        Db = #db{server=Server, name = <<"couchbeam_testdb">>, options=[]},

        ?assertEqual({error, not_found}, couchbeam_view:fetch(Db, {"couchbeam", "test"}, [sync_query])),
        ok
    after
        couchbeam_mocks:teardown()
    end.

%% Helper to generate mock view responses based on URL
view_mock_response(Url) ->
    UrlBin = iolist_to_binary(Url),
    Ref = make_ref(),
    %% Check for limit=1 (used by first/3)
    HasLimit1 = case binary:match(UrlBin, <<"limit=1">>) of
        nomatch -> false;
        _ -> true
    end,
    Response = case binary:match(UrlBin, <<"_all_docs">>) of
        nomatch ->
            %% Named view query - check for options
            case binary:match(UrlBin, <<"start_key">>) of
                nomatch ->
                    %% Basic view query - return docs where type == "test"
                    case HasLimit1 of
                        true ->
                            %% first/3 - return single row
                            {[
                                {<<"total_rows">>, 2},
                                {<<"offset">>, 0},
                                {<<"rows">>, [
                                    {[{<<"id">>, <<"doc1">>}, {<<"key">>, <<"doc1">>},
                                      {<<"value">>, {[{<<"_id">>, <<"doc1">>}]}},
                                      {<<"doc">>, {[{<<"_id">>, <<"doc1">>}, {<<"type">>, <<"test">>}]}}]}
                                ]}
                            ]};
                        false ->
                            %% Normal query - return 2 docs
                            {[
                                {<<"total_rows">>, 2},
                                {<<"offset">>, 0},
                                {<<"rows">>, [
                                    {[{<<"id">>, <<"doc1">>}, {<<"key">>, <<"doc1">>},
                                      {<<"value">>, {[{<<"_id">>, <<"doc1">>}]}},
                                      {<<"doc">>, {[{<<"_id">>, <<"doc1">>}, {<<"type">>, <<"test">>}]}}]},
                                    {[{<<"id">>, <<"doc2">>}, {<<"key">>, <<"doc2">>},
                                      {<<"value">>, {[{<<"_id">>, <<"doc2">>}]}},
                                      {<<"doc">>, {[{<<"_id">>, <<"doc2">>}, {<<"type">>, <<"test">>}]}}]}
                                ]}
                            ]}
                    end;
                _ ->
                    %% Query with start_key - check for end_key
                    case binary:match(UrlBin, <<"end_key">>) of
                        nomatch ->
                            %% Just start_key - return 4 docs
                            {[
                                {<<"total_rows">>, 4},
                                {<<"offset">>, 0},
                                {<<"rows">>, [
                                    {[{<<"id">>, <<"test1">>}, {<<"key">>, <<"test1">>}, {<<"value">>, 1}]},
                                    {[{<<"id">>, <<"test2">>}, {<<"key">>, <<"test2">>}, {<<"value">>, 2}]},
                                    {[{<<"id">>, <<"test3">>}, {<<"key">>, <<"test3">>}, {<<"value">>, 3}]},
                                    {[{<<"id">>, <<"test4">>}, {<<"key">>, <<"test4">>}, {<<"value">>, 4}]}
                                ]}
                            ]};
                        _ ->
                            %% start_key and end_key - return 3 docs
                            {[
                                {<<"total_rows">>, 3},
                                {<<"offset">>, 0},
                                {<<"rows">>, [
                                    {[{<<"id">>, <<"test1">>}, {<<"key">>, <<"test1">>}, {<<"value">>, 1}]},
                                    {[{<<"id">>, <<"test2">>}, {<<"key">>, <<"test2">>}, {<<"value">>, 2}]},
                                    {[{<<"id">>, <<"test3">>}, {<<"key">>, <<"test3">>}, {<<"value">>, 3}]}
                                ]}
                            ]}
                    end
            end;
        _ ->
            %% _all_docs - return 3 docs
            {[
                {<<"total_rows">>, 3},
                {<<"offset">>, 0},
                {<<"rows">>, [
                    {[{<<"id">>, <<"_design/couchbeam">>}, {<<"key">>, <<"_design/couchbeam">>}, {<<"value">>, {[]}}]},
                    {[{<<"id">>, <<"doc1">>}, {<<"key">>, <<"doc1">>}, {<<"value">>, {[]}}]},
                    {[{<<"id">>, <<"doc2">>}, {<<"key">>, <<"doc2">>}, {<<"value">>, {[]}}]}
                ]}
            ]}
    end,
    couchbeam_mocks:set_body(Ref, Response),
    {ok, 200, [], Ref}.

-endif.
