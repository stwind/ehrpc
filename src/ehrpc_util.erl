-module(ehrpc_util).

-export([get_env/1]).
-export([get_env/2]).

%% ===================================================================
%% Public
%% ===================================================================

get_env(Keys) ->
    get_env(Keys, undefined).

get_env(Key, Default) when is_atom(Key) ->
    get_env({Key}, Default);
get_env(Keys, Default) ->
    [First | Rest] = tuple_to_list(Keys),
    case application:get_env(ehrpc, First)  of
        {ok, Value} ->
            do_get_env(Rest, Value, Default);
        undefined ->
            Default
    end.

%% ===================================================================
%% Private
%% ===================================================================

do_get_env([Key | Rest], Configs, Default) ->
    case proplists:get_value(Key, Configs, Default) of
        undefined ->
            Default;
        Value ->
            do_get_env(Rest, Value, Default)
    end;
do_get_env([], Value, _) ->
    Value.
