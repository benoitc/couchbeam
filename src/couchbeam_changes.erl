-module(couchbeam_changes).

-include("couchbeam.hrl").
-export([continuous_acceptor/2]).

-record(state, {
    partial_chunk = <<"">>
}).

%% @doc initiate continuous loop 
continuous_acceptor(Pid, PidRef) ->
    receive
        {ibrowse_req_id, PidRef, IbrowseRef} ->
            continuous_acceptor(Pid, PidRef, IbrowseRef,
                #state{})
    after ?DEFAULT_TIMEOUT ->
        Pid ! {PidRef, {error, {timeout, []}}}
    end.


%% @doc main ibrowse loop to receive continuous changes 
continuous_acceptor(Pid, PidRef, IbrowseRef, State) ->
    receive
        {ibrowse_async_response_end, IbrowseRef} ->
            Pid ! {PidRef, done};
        {ibrowse_async_response, IbrowseRef, {error,Error}} ->
            Pid ! {PidRef, {error, Error}};
        {ibrowse_async_response, IbrowseRef, Chunk} ->
            Messages = [M || M <- re:split(Chunk, ",?\n", [trim]), M =/= <<>>],
            {ok, State1} = handle_messages(Messages, Pid, PidRef, IbrowseRef, State),
            continuous_acceptor(Pid, PidRef, IbrowseRef, State1);
        {ibrowse_async_headers, IbrowseRef, Status, Headers} ->
            if Status =/= "200" ->
                    Pid ! {PidRef, {error, {Status, Headers}}};
                true ->
                    io:format("got header~n"),
                    ibrowse:stream_next(IbrowseRef), 
                    continuous_acceptor(Pid, PidRef, IbrowseRef, State)
                    
            end 
    end.

handle_messages([], _Pid, _PidRef, IbrowseRef, State) ->
    ibrowse:stream_next(IbrowseRef),
    {ok, State};
handle_messages([<<"{\"last_seq\":", LastSeq/binary>>], Pid, PidRef,
        IbrowseRef, State) ->
    %% end of continuous response
    Pid ! {PidRef, {last_seq, LastSeq}},
    ibrowse:stream_next(IbrowseRef),
    {ok, State};
handle_messages([Chunk|Rest], Pid, PidRef, IbrowseRef, State) ->
    #state{partial_chunk=Partial}=State,
    NewState = try
        ChangeRow = decode_row(<<Partial/binary, Chunk/binary>>),
        Pid! {PidRef, {change, ChangeRow}},
        #state{}
    catch
    throw:{invalid_json, Bad} ->
        State#state{partial_chunk = Bad}
    end,
    handle_messages(Rest,  Pid, PidRef, IbrowseRef, NewState).

decode_row(<<",", Rest/binary>>) ->
    decode_row(Rest);
decode_row(Row) ->
    couchbeam_util:json_decode(Row).

