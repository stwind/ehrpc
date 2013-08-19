-module(ehrpc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = ehrpc_sup:start_link(),
    setup_cowboy(),
    {ok, Sup}.

stop(_State) ->
    ok.

%% ===================================================================
%% Private
%% ===================================================================

setup_cowboy() ->
    NbAcceptors = ehrpc_util:get_env({server, size}, 100),
    {ok, _} = cowboy:start_http(ehrpc, NbAcceptors, trans_opts(), proto_opts()).

trans_opts() ->
    {ok, Ip} = inet_parse:address(ehrpc_util:get_env(ip, "0.0.0.0")),
    [
        {port, ehrpc_util:get_env(port, 5566)},
        {ip, Ip}, 
        {max_connections, ehrpc_util:get_env(max_connections, 1024)}, 
        {backlog, ehrpc_util:get_env(backlog, 1024)}
    ].

proto_opts() ->
    DispatchFile = ehrpc_util:get_env(dispatch_file, "priv/dispatch.script"),
    {ok, Dispatch} = file:script(DispatchFile, bs()),
    [{env, [{dispatch, cowboy_router:compile(Dispatch)}]}].

bs() ->
    lists:foldl(fun({K,V}, Bs) ->
                erl_eval:add_binding(K, V, Bs)
        end, erl_eval:new_bindings(), []).
