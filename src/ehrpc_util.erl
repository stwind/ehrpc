-module(ehrpc_util).

-export([get_env/2]).

%% ===================================================================
%% Public
%% ===================================================================

get_env(Type, Key) ->
    {ok, Configs} = application:get_env(ehrpc, Type),
    proplists:get_value(Key, Configs).
