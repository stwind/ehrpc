-module(rpc_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 5000).

-define(WITH_CLIENT(Count, Case), {setup,
        fun() -> start_clients(Count) end,
        fun(Nodes) -> [slave:stop(N) || N <- Nodes] end,
        fun(Nodes) -> ?_test(Case(Nodes)) end
    }).

setup() ->
    {ok, _} = net_kernel:start(['server@127.0.0.1', longnames]),
    ehrpc_env(node(), server),
    start_apps(node()).

cleanup(_) ->
    net_kernel:stop(),
    application:stop(ehrpc).

start_clients(Count) ->
    Name = fun(N) -> list_to_atom("client" ++ integer_to_list(N)) end,
    [new_client(Name(N)) || N <- lists:seq(1, Count)].

ehrpc_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
            {"call", ?WITH_CLIENT(1, pingpong)},
            {"error return", ?WITH_CLIENT(1, error_return)},
            {"throw", ?WITH_CLIENT(1, throw_return)},
            {"erlang error", ?WITH_CLIENT(1, erlang_error)}
        ]}.

%% ===================================================================
%% Test cases
%% ===================================================================

pingpong([Client]) ->
    ?assertMatch(pong, call_server(Client, ping, [])),
    ?assertMatch(ping, call_server(Client, pong, [])),
    ?assertMatch(pong, call_server(Client, ping, [])),
    ?assertMatch(ping, call_server(Client, pong, [])).

error_return([Client]) ->
    ?assertMatch({error, whatever}, 
        call_server(Client, error_return, [whatever])),
    ?assertMatch({error, {type, value}}, 
        call_server(Client, error_return, [{type, value}])).

throw_return([Client]) ->
    ?assertMatch({error, whatever},
        call_server(Client, throw, [{error, whatever}])),
    ?assertMatch({error, {type, value}},
        call_server(Client, throw, [{error, {type, value}}])).

erlang_error([Client]) ->
    ?assertMatch({error, badarg}, call_server(Client, error, [badarg])).

%% ===================================================================
%% Helpers
%% ===================================================================

new_client(Name) ->
    Args = "-env ERL_LIBS ../deps -pa ../ebin",
    {ok, Node} = slave:start('127.0.0.1', Name, Args),
    ehrpc_env(Node, client),
    start_apps(Node),
    Node.

ehrpc_env(Node, server) ->
    call(Node, application, set_env, [ehrpc, server, [
                {host, "127.0.0.1"},
                {port, 5566},
                {middlewares, [ehrpc_mfa]},
                {size, 5}
            ]]);
ehrpc_env(Node, client) ->
    call(Node, application, set_env, [ehrpc, client, [
                {services, [
                        {p1, [
                                {url, ["http://localhost:5566"]}
                            ]}
                    ]}
            ]]).

call_server(Client, Fun, Args) ->
    call(Client, ehrpc, call, [p1, server, Fun, Args, ?TIMEOUT]).

call(Node, Mod, Fun, Args) ->
    rpc:call(Node, Mod, Fun, Args).

start_apps(Node) ->
    call(Node, application, start, [crypto]),
    ok = call(Node, application, start, [public_key]),
    ok = call(Node, application, start, [ssl]),
    ok = call(Node, application, start, [ranch]),
    ok = call(Node, application, start, [cowboy]),
    ok = call(Node, application, start, [lhttpc]),
    ok = call(Node, application, start, [ehrpc]).
