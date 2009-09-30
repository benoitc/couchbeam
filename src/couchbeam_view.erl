%%% Copyright 2009 Benoît Chesneau.
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%% @author Benoît Chesneau <benoitc@e-engura.org>
%% @copyright 2009 Benoît Chesneau.



-module(couchbeam_view).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-behaviour(gen_server).

-include("couchbeam.hrl").

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2,
         handle_info/2]).
-export([fetch_view/1, fetch_view/2, parse_view/1, parse_view/2,
         count/1, count/2]).
-export([close_view/1]).
                  
                  
fetch_view(ViewPid) ->
    fetch_view(ViewPid, false).
fetch_view(ViewPid, Refresh) ->
    gen_server:call(ViewPid, {fetch_view, Refresh}, infinity).
 

%% @spec parse_view(json_object()) -> view_result()
%% @type view_result() = {TotalRows::integer(), Offset::interger(), Rows::rows()}
%% @type rows() = {Id::binary(), Key::term(), Row::proplist()}
%% @doc Return a list of document ids for a given view.   
parse_view(ViewPid) ->
    parse_view(ViewPid, false).
parse_view(ViewPid, Refresh) ->
    gen_server:call(ViewPid, {parse_view, Refresh}, infinity).
    
count(ViewPid) ->
    count(ViewPid, false).
count(ViewPid, Refresh) ->
    gen_server:call(ViewPid, {count, Refresh}, infinity).
                     
close_view(ViewPid) ->
    catch exit(ViewPid, kill),
    ok.

%%---------------------------------------------------------------------------
%% gen_server callbacks
%%---------------------------------------------------------------------------
%% @private                 
init({Vname, Params, #db{server=ServerState, couchdb=CouchdbParams, base=BaseDB}=DbState}) -> 
    Base = case Vname of
        '_all_docs' ->
            io_lib:format("~s/_all_docs", [BaseDB]);
        '_all_docs_by_seq' ->
            io_lib:format("~s/_all_docs_by_seq", [BaseDB]);
        {DName, VName1} ->
            io_lib:format("~s/_design/~s/_view/~s", [BaseDB, DName, VName1])
    end,
    ViewState = #view{server    = ServerState, 
                      couchdb   = CouchdbParams, 
                      db        = DbState,
                      name      = Vname,
                      base      = Base, 
                      params    = Params},
    {ok, ViewState}.
    
handle_call({fetch_view, Refresh}, _From, State) ->
    {ViewResults, NewState} = is_fetch_view(Refresh, State),
    {reply, ViewResults, NewState};
    
handle_call({parse_view, Refresh}, _From, State) ->
    {ViewResults, NewState} = is_fetch_view(Refresh, State),
    #view{fetched=Fetched} = NewState,
    Parsed = case Fetched of
        true -> parse_view1(NewState);
        false -> ViewResults
    end,
    {reply, Parsed, NewState};
    
handle_call({count, Refresh}, _From, State) ->
    {_, NewState} = is_fetch_view(Refresh, State),
    #view{fetched=Fetched, rows=Rows} = NewState,
    Count = case Fetched of
        true -> length(Rows);
        false -> 0
    end,
     {reply, Count, NewState};
    
handle_call(stop_view, _From, State) ->
    {stop, ok, State}.
    
handle_cast(_Msg, State) ->
    {no_reply, State}.
    

handle_info({'EXIT', _Pid, _Reason}, State) ->
    io:format("Stopping view ~p ~n", [State#view.name]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private

is_fetch_view(Refresh, #view{couchdb=C, base=Base, params=Params, 
                            fetched=Fetched, view_cache=ViewCache}=State) ->
    ViewResults = case Refresh of
        true ->
            fetch_view(C, Params, Base);
        false when(Fetched =:= false) ->
            fetch_view(C, Params, Base);
        false -> ViewCache
    end,
    NewState = update_view_state(ViewResults, Refresh, State),
    {ViewResults, NewState}.
    

fetch_view(C, Params, Base) ->
    case proplists:get_value("keys", Params) of
        undefined -> 
            couchbeam_resource:get(C, Base, [], Params, []);
        Keys ->
            Params1 = proplists:delete("keys", Params),
            Body = couchbeam:json_encode({[{<<"keys">>, Keys}]}),
            couchbeam_resource:post(C, Base, [], Params1, Body, [])
    end.
    
update_view_state(ViewResults, Refresh, #view{fetched=Fetched}=ViewState) ->
    if 
        Refresh =:= false, Fetched =:= true ->
            %% don't do unnecessary things
            ViewState;
        true ->
            case ViewResults of
                {ok, Results} ->
                    update_view_state1(Results, ViewState);
                {error, _} ->
                    reset_view_state(ViewState)
            end
        end.
      
update_view_state1({ViewProps}=ViewResult, ViewState) ->  
    TotalRows = proplists:get_value(<<"total_rows">>, ViewProps, 0),
    Offset = proplists:get_value(<<"offset">>, ViewProps, 0),
    Rows = proplists:get_value(<<"rows">>, ViewProps, []),
    {Meta, Rows} = case  proplists:get_value(<<"rows">>, ViewProps) of
        undefined ->
            {ViewProps, []};
        Rows1 ->
            ViewProps1 = proplists:delete(<<"rows">>, ViewProps),
            {ViewProps1, Rows1}
    end,    
    ViewState1 = ViewState#view{fetched  = true,
                       total_rows = TotalRows,
                       offset = Offset,
                       rows = Rows,
                       meta = Meta,
                       view_cache = ViewResult},
    ViewState1;

update_view_state1(ViewResult, ViewState) -> 
    ViewState1 = reset_view_state(ViewState),
    ViewState2 = ViewState1#view{fetched = true,
                                 view_cache = ViewResult},
    ViewState2.
    
reset_view_state(ViewState) ->
    ViewState1 = ViewState#view{fetched  = false,
                                total_rows = 0,
                                offset = 0,
                                rows = [],
                                meta = []},
    ViewState1.

parse_view1(#view{view_cache={_ViewCache}, total_rows=TotalRows,offset=Offset, rows=Rows, meta=Meta}) ->
    Rows1 = [begin
        {Row1} = Row,
        Id = proplists:get_value(<<"id">>, Row1),
        Key = proplists:get_value(<<"key">>, Row1),
        case proplists:get_value(<<"value">>, Row1) of
        [] -> Id;
        {Value} -> {Id, Key, {Value}};
        Value when is_list(Value) -> {Id, Key, Value};
        Value when is_integer(Value) -> {Id, Key, Value};
        Value when is_binary(Value) -> {Id, Key, Value};
        _ -> Id
        end
    end || Row <- Rows],
    {TotalRows, Offset, Meta, Rows1};
parse_view1(#view{view_cache=ViewCache, total_rows=_TotalRows,offset=_Offset, rows=_Rows, meta=_Meta}) ->
    ViewCache.
