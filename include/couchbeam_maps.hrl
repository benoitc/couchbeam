%% New type definitions for map-based API
%% This file defines map-based types alongside the existing proplist types

-type json_obj() :: map().
-type json_arr() :: list(json_term()).
-type json_term() :: json_obj() | json_arr() | binary() | number() | boolean() | null.

%% Document type using maps
-type doc_map() :: #{binary() => json_term()}.

%% View row using maps
-type view_row_map() :: #{
    <<"id">> => binary(),
    <<"key">> => json_term(),
    <<"value">> => json_term(),
    <<"doc">> => doc_map()
}.

%% Changes row using maps
-type change_row_map() :: #{
    <<"seq">> => integer() | binary(),
    <<"id">> => binary(),
    <<"changes">> => [#{<<"rev">> => binary()}],
    <<"deleted">> => boolean(),
    <<"doc">> => doc_map()
}.

%% Streaming result wrapper
-type stream_result(T) :: {stream, T} | {batch, [T]} | {done, term()}.