-module(bitbucket_config).

-export([ load/1 ]).

-compile([{parse_transform, lager_transform}]).

-spec load(string()) -> ok | {error, term()}.
load(Path) ->
    case file:consult(Path) of
        {ok, Config} ->
            lager:info("Reading config file ~p.~n", [Path]),
            [ok = application:set_env(bec, K, V) || {K, V} <- Config],
            ok;
        {error, Reason} ->
            lager:error( "Error while reading config file ~p (~p).~n"
                       , [Path, Reason]),
            {error, Reason}
    end.
