-module(bitbucket_config).

-export([ load/1 ]).

-include_lib("kernel/include/logger.hrl").

-spec load(string()) -> ok | {error, term()}.
load(Path) ->
    case file:consult(Path) of
        {ok, Config} ->
            ?LOG_INFO("Reading config file ~p.~n", [Path]),
            set_vars(Config);
        {error, Reason} ->
            %% Error will be printed higher up in the call chain
            {error, Reason}
    end.


set_vars([]) ->
    ok;
set_vars([{K, V}|Rest]) when is_list(V) ->
    application:set_env(bec, K, V),
    set_vars(Rest);
set_vars([{K, {env, EnvVar}}|Rest]) ->
    case os:getenv(EnvVar) of
        false ->
            {error, {missing_env_var, EnvVar}};
        V ->
            set_vars([{K, V}|Rest])
    end.
