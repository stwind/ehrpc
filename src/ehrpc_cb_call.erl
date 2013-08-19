-module(ehrpc_cb_call).

-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([service_available/2]).
-export([malformed_request/2]).
-export([content_types_accepted/2]).
-export([from_bert/2]).

-record(ctx, {
        req,
        middlewares = [] :: list(module())
    }).

%% ===================================================================
%% Callbacks
%% ===================================================================

init({_, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) ->
    Middlewares = ehrpc_util:get_env({server, middlewares}),
    {ok, Req, #ctx{middlewares = Middlewares}}.

service_available(Req, Ctx) ->
    {true, Req, Ctx}.

allowed_methods(Req, Ctx) ->
    {[<<"POST">>], Req, Ctx}.

malformed_request(Req, Ctx) ->
    case cowboy_req:body(Req) of
        {ok, Body, Req1} ->
            case catch ehrpc_proto:decode(Body) of
                {'EXIT', _} ->
                    {true, err_body(invalid_request, Req), Ctx};
                {call, Mod, Fun, Args} ->
                    {false, Req1, Ctx#ctx{req = ehrpc_req:new(call, {Mod, Fun, Args})}}
            end;
        {error, Reason} ->
            {true, err_body(Reason, Req), Ctx}
    end.

content_types_accepted(Req, Ctx) ->
    {[{'*', from_bert}], Req, Ctx}.

from_bert(Req, #ctx{req = RpcReq, middlewares = Middlewares} = Ctx) ->
    RpcReq1 = run_middlewares(RpcReq, Middlewares),
    {true, reply(RpcReq1, Req), Ctx}.

%% ===================================================================
%% Callbacks
%% ===================================================================

err_body(Reason, Req) ->
    Body = ehrpc_proto:encode(ehrpc_proto:error(Reason)),
    cowboy_req:set_resp_body(Body, Req).

reply(RpcReq, Req) ->
    Body = ehrpc_proto:encode(ehrpc_proto:reply(ehrpc_req:resp(RpcReq))),
    cowboy_req:set_resp_body(Body, Req).

run_middlewares(Req, [Mod | Rest]) ->
    case Mod:execute(Req) of
        {ok, Req1} ->
            run_middlewares(Req1, Rest);
        {error, _Reason} ->
            nop
    end;
run_middlewares(_, []) ->
    ok.
