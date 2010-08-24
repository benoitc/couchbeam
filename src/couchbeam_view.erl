%%% Copyright 2010 Benoît Chesneau.
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

-module(couchbeam_view).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-behaviour(gen_server).

-include("couchbeam.hrl").

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2,
         handle_info/2]).
-export([count/1, count/2, fetch/1, fetch/2, first/1, first/2, 
        fold/2, fold/3, foreach/2, foreach/3]).
-export([start_view/2, close/1]).

%% @type json_string() = atom | binary()
%% @type json_number() = integer() | float()
%% @type json_array() = [json_term()]
%% @type json_object() = {struct, [{json_string(), json_term()}]}
%% @type json_term() = json_string() | json_number() | json_array() |
%%                     json_object()

%% @spec start_view(Db, ViewName) -> ViewPid
%% @doc start a view supervised
%% ViewName could be "_all_docs" or with the form "DesignName/ViewName"
start_view(Db, ViewName) ->
    ViewName1 = view_name(ViewName),
    View = {
        make_view_id(Db, ViewName1),
        {gen_server, start_link,
            [?MODULE, [Db, ViewName1], []]},
        permanent,
        1000,
        worker,
        [?MODULE]
    },
    case supervisor:start_child(couchbeam_view_sup, View) of
    {ok, Pid} ->
        Pid;
    {error, already_present} ->
        case supervisor:restart_child(couchbeam_view_sup, View) of
        {ok, Pid} ->
            Pid;
        {error, running} ->
            {error, {already_started, Pid}} =
                supervisor:start_child(couchbeam_view_sup, View),
            Pid
        end;
    {error, {already_started, Pid}} ->
        Pid
    end.

%% spec close(ViewPid:pid) -> void()
%% @doc close view pid
close(ViewPid) ->
    try
        gen_server:call(ViewPid, close)
    catch
        exit:_ -> ok
    end.

%% spec count(ViewPid:pid()) -> Integer
%% @doc get number of results in the view
count(ViewPid) ->
    count(ViewPid, []).

%% spec count(ViewPid, Options) -> Result
%% ViewPid = pid()
%% Options = ViewOptions
%% Result = Integer
%% Options = [ViewOptions]
%% @doc get number of results in the view

count(ViewPid,  Options) ->
     Fetch = gen_server:call(ViewPid, {fetch, Options}, infinity),
     case Fetch of
        {ok, {Results}} ->
            case proplists:get_value(<<"limit">>, Options) of
                0 ->
                    proplists:get_value(<<"total_rows">>, Results);
                _ ->
                    Rows = proplists:get_value(<<"rows">>, Results),
                    length(Rows)
            end;
        {error, _} ->
            Fetch
    end.

%% spec first(ViewPid) -> Result
%% ViewPid = pid()
%% Result = json_term()
%% @doc get iall results of the view
fetch(ViewPid) ->
    fetch(ViewPid, []).

%% spec fetch(ViewPid, Options) -> Result
%% ViewPid = pid()
%% Options = ViewOptions
%% Result = json_term()
%% Options = [ViewOptions]
%% @doc get all results in a view.
fetch(ViewPid, Options) ->
    gen_server:call(ViewPid, {fetch, Options}, infinity).


%% spec first(ViewPid) -> Result
%% ViewPid = pid()
%% Result = json_term()
%% @doc get first row in results of the view
first(ViewPid) ->
    first(ViewPid, []).

%% spec first(ViewPid, Options) -> Result
%% ViewPid = pid()
%% Options = ViewOptions
%% Result = json_term()
%% Options = [ViewOptions]
%% @doc get first row in results of the view
first(ViewPid, Options) ->
    % make sure we don't override the limit
    Options1 = case proplists:get_value("limit", Options) of
        undefined ->
            [{"limit", 1}|Options];
        _Else ->
            Opts = proplists:delete("limit", Options),
            [{"limit", 1}|Opts]
    end,

    Fetch = gen_server:call(ViewPid, {fetch, Options1}, infinity),
    case Fetch of
        {ok, {Results}} ->
            Rows = proplists:get_value(<<"rows">>, Results),
            [FirstRow|_] = Rows,
            FirstRow;
        {error, _} ->
            Fetch
    end.


%% spec fold(ViewPid, Fun) -> Acc
%% ViewPid = pid()
%% Fun = fun(Row, AccIn) -> AccOut
%% Row = json_term()
%% Acc = AccIn = AccOut = term()
%% @doc like fold/3 but without options given to the view
fold(ViewPid, Fun) ->
    fold(ViewPid, Fun, []).

%% spec fold(ViewPid, Fun, Options) -> Acc
%% ViewPid = pid()
%% Fun = fun(Row, AccIn) -> AccOut
%% Row = json_term()
%% Options = ViewOptions
%% Acc = AccIn = AccOut = term()
%% Options = [ViewOptions]
%% @doc Calls Fun on successive keys and values of View Results. Fun must return a new accumulator 
%% which is passed to the next call. [] is returned if the list is empty.
%% The evaluation order is undefined.
fold(ViewPid, Fun, Options) ->
    Fetch = gen_server:call(ViewPid, {fetch, Options}, infinity),
    case Fetch of
        {ok, {Results}} ->
            Rows = proplists:get_value(<<"rows">>, Results),
            fold_fun(Rows, Fun, []);
        {error, _} ->
            Fetch
    end.

%% spec foreah(ViewPid, Fun) -> void()
%% ViewPid = pid()
%% Fun = fun(Row) -> void()
%% Row = json_term()
%% like foreach/3 but without options given to the view
foreach(ViewPid, Fun) ->
    foreach(ViewPid, Fun, []).

%% spec foreah(ViewPid, Fun, Options) -> void()
%% ViewPid = pid()
%% Fun = fun(Row) -> void()
%% Row = json_term()
%% Options = ViewOptions
%% Options = [ViewOptions]
%% @doc Calls Fun(Elem) for each element Row in view results. This function is used 
%% for its side effects and the evaluation order is defined to be the
%% same as the order of the elements in the results.
foreach(ViewPid, Fun, Options) ->
    Fetch = gen_server:call(ViewPid, {fetch, Options}, infinity),
    case Fetch of
        {ok, {Results}} ->
            Rows = proplists:get_value(<<"rows">>, Results),
            do_foreach(Rows, Fun);
        {error, _} ->
            Fetch
    end.

%%---------------------------------------------------------------------------
%% gen_server callbacks
%%---------------------------------------------------------------------------

init([#db{base=Base}=Db, ViewName]) ->
    Uri = case ViewName of
        "_all_docs" ->
            io_lib:format("~s/_all_docs", [Base]);
        _Else ->
            case string:tokens(ViewName, "/") of
            [DName, VName] ->
                io_lib:format("~s/_design/~s/_view/~s", [Base, DName,
                        VName]);
            _ ->
                throw(invalid_view_name)
            end
    end,
    {ok, #view{db=Db, 
                view_name=ViewName,
                view_uri= Uri}}.

handle_call({fetch, Options}, _From, #view{db=Db, view_uri=Uri}=View) ->
    case parse_view_options(Options, []) of
        fail ->
            {reply, {error, invalid_view_options}, View};
        Options1 ->
            #db{couchdb=C}=Db,
            Results = case proplists:get_value("keys", Options1) of
                undefined ->
                    couchbeam_resource:get(C, Uri, [], Options1, []);
                Keys ->
                    Options2 = proplists:delete("keys", Options1),
                    Payload = couchbeam:json_encode({[{<<"keys">>,
                                    Keys}]}),
                    couchbeam_resource:post(C, Uri, [], Options2,
                        Payload,[])
            end,
            {reply, Results, View}
    end;
handle_call(close, _From, View) ->
    {stop, normal, View}.

handle_cast(_Msg,  State) ->
    {no_reply, State}.
    

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% @private
do_foreach([], _Fun) ->
    ok;
do_foreach([Row|Rest], Fun) ->
    Fun(Row),
    do_foreach(Rest, Fun).

fold_fun([], _Fun, Acc) ->
    Acc;
fold_fun([Row|Rest], Fun, Acc) ->
    fold_fun(Rest, Fun, Fun(Row, Acc)).

parse_view_options([], Acc) ->
    Acc;
parse_view_options([V|Rest], Acc) when is_atom(V) ->
    parse_view_options(Rest, [{atom_to_list(V), true}|Acc]);
parse_view_options([{K,V}|Rest], Acc) when is_list(K) ->    
    parse_view_options(Rest, [{K,V}|Acc]);
parse_view_options([{K,V}|Rest], Acc) when is_binary(K) ->
    parse_view_options(Rest, [{binary_to_list(K),V}|Acc]);
parse_view_options([{K,V}|Rest], Acc) when is_atom(K) ->   
    parse_view_options(Rest, [{atom_to_list(K),V}|Acc]);
parse_view_options(_,_) ->
    fail.

make_view_id(#db{name=DbName}, VName) ->
    Md5 = crypto:md5(DbName ++ VName),
    couchbeam_util:to_hex(Md5). 

view_name(VName) ->
    couchbeam_util:to_list(VName).
