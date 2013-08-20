-module(ehrpc_req).

-export([new/2]).
-export([mfa/1]).
-export([reply/2]).
-export([meta/2]).
-export([meta/3]).
-export([resp/1]).
-export([set_meta/3]).

-record(req, {
        type :: atom(),
        mfa :: {module(), function(), list(term())},
        resp :: binary(),
        meta = dict:new() :: dict()
    }).

%% ===================================================================
%% Public
%% ===================================================================

new(Type, MFA) ->
    Req = #req{type = Type, mfa = MFA},
    set_meta(start_time, os:timestamp(), Req).

mfa(#req{mfa = {M, F, A}, type = Type}) ->
    {Type, M, F, A}.

reply(Reply, Req) ->
    Req1 = set_meta(end_time, os:timestamp(), Req),
    Req1#req{resp = Reply}.

meta(Key, Req) ->
    meta(Key, Req, undefined).

meta(Key, #req{meta = Meta}, Default) ->
    case dict:find(Key, Meta) of
        {ok, Val} -> Val;
        error -> Default
    end.

resp(#req{resp = Resp}) ->
    Resp.

set_meta(Key, Val, #req{meta = Meta} = Req) ->
    Req#req{meta = dict:store(Key, Val, Meta)}.

%% ===================================================================
%% Public
%% ===================================================================
