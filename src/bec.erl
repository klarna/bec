-module(bec).

-export([ main/1 ]).

-spec main([string()]) -> ok.
main([]) ->
    usage();
main(Args) ->
    Specs = specs(),
    case getopt:parse(Specs, Args) of
        {ok, {Options, []}} ->
            case lists:member(help, Options) of
                true  ->
                    usage(Specs);
                false ->
                    do_main(Options)
            end;
        {ok, {_Options, NonOptArgs}} ->
            io:format( "Non valid arguments found: ~p~n", [NonOptArgs]),
            usage(Specs);
        {error, {Reason, Data}} ->
            io:format( "Error: ~s ~p~n~n", [Reason, Data]),
            usage(Specs)
    end.

do_main(Options) ->
    Config = proplists:get_value(config, Options),
    Verbosity = proplists:get_value(verbosity, Options),
    set_logging(verbosity_level(Verbosity)),
    application:ensure_all_started(bec),
    case bitbucket_config:load(Config) of
        ok ->
            case proplists:get_value(repo_config, Options) of
                undefined ->
                    io:format( "Please specify a repo_config.~n", []),
                    usage();
                RepoConfig ->
                    Enforce = proplists:get_value(enforce, Options),
                    K       = proplists:get_value(keep, Options),
                    case bitbucket_repo_config:verify( RepoConfig,
                                                       [ {enforce, Enforce}
                                                       , {abort_on_error, not K}
                                                       ]) of
                        true ->
                            ok;
                        false ->
                            %% Give lager an opportunity to flush its buffers
                            application:stop(lager),
                            case Enforce of
                                true ->
                                    ok;
                                false ->
                                    erlang:halt(1)
                            end
                    end
            end;
        {error, Reason} ->
            io:format("Could not read config file ~p: (~p).~n", [ Config
                                                                , Reason
                                                                ]),
            {error, Reason}
    end.

specs() ->
    [ { help,        $h, "help",        undefined
      , "Show this help message"}
    , { config,      $c, "config",      {string, "bitbucket.config"}
      , "The BitBucket Config File"}
    , { repo_config, $r, "repo_config", {string, undefined}
      , "The Repo Config to check or configure"}
    , { enforce,     $e, "enforce",     {boolean, false}
      , "Enforce values when they do not match expectations"}
    , { keep,        $k, "keep",        {boolean, false}
      , "Keep going after the first error (always true when enforce == true)"}
    , { verbosity,   $v, "verbosity",   {integer, 1}
      , "Verbosity Level"}
    ].

usage() ->
    usage(specs()).

usage(Specs) ->
    getopt:usage(Specs, escript:script_name()).

set_logging(Verbosity) ->
    ok = application:load(lager),
    %% Starting from OTP 21, error logger is not started by default any longer.
    %% See: https://github.com/erlang-lager/lager/issues/452
    ok = application:set_env(lager, error_logger_redirect, false),
    ok = application:set_env(lager, handlers, [ { lager_console_backend
                                                , [{level, Verbosity}] }
                                              ]),
    ok.

verbosity_level(0) ->
    error;
verbosity_level(1) ->
    warning;
verbosity_level(2) ->
    info;
verbosity_level(I) when is_integer(I), I > 2 ->
    debug.
