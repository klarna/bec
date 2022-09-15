-module(bitbucket_config).

-export([ load/1 ]).

-include_lib("kernel/include/logger.hrl").

-spec load(string()) -> ok | {error, term()}.
load(Path) ->
    case file:consult(Path) of
        {ok, Config} ->
            ?LOG_INFO("Reading config file ~p.~n", [Path]),
            [ok = application:set_env(bec, K, V) || {K, V} <- Config],
            ok;
        {error, Reason} ->
            %% Error will be printed higher up in the call chain
            {error, Reason}
    end.
