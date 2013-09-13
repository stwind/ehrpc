-module(ehrpc).

-export([call/5]).

%% ===================================================================
%% Public
%% ===================================================================

call(Service, Mod, Fun, Args, Timeout) ->
    Body = ehrpc_proto:encode(ehrpc_proto:call(Mod, Fun, Args)),
    Url = url(Service),
    case catch lhttpc:request(Url ++ "/call", post, [], Body, Timeout) of
        {'EXIT', {econnrefused, _}} ->
            {error, {system_busy, Url}};
        {'EXIT', Error} ->
            {error, {Url, Error}};
        {ok, {{200, _}, _, RespBody}} ->
            case ehrpc_proto:decode(RespBody) of
                {error, Reason} ->
                    {error, reason(Reason)};
                Other ->
                    Other
            end;
        {ok, {{Status, _}, _, _}} ->
            {error, {Url, Status}};
        {error, Reason} ->
            {error, {Url, Reason}}
    end.

%% ===================================================================
%% Private
%% ===================================================================

url(Service) ->
    Urls = ehrpc_util:get_env({client, services, Service, url},["http://localhost:5566"]),
    random:seed(),
    Idx = random:uniform(length(Urls)),
    lists:nth(Idx, Urls).

reason({server, _, Type, undefined, _}) ->
    Type;
reason({server, _, Type, Value, _}) ->
    {Type, Value}.
