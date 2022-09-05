-module(bec).

-compile([{parse_transform, lager_transform}]).

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
            usage(Specs),
            print_error_and_exit("Non valid arguments found: ~p~n", [NonOptArgs]);
        {error, {Reason, Data}} ->
            usage(Specs),
            print_error_and_exit("Error: ~s ~p~n~n", [Reason, Data])
    end.

-spec print_error_and_exit(Fmt :: io:format(), Args :: [term()]) -> no_return().
print_error_and_exit(Fmt, Args) ->
    lager:error(Fmt, Args),
    flush_and_exit(1).

flush_and_exit(Code) ->
    application:stop(lager),
    erlang:halt(Code).

do_main(Options) ->
    Config = proplists:get_value(config, Options),
    Verbosity = proplists:get_value(verbosity, Options),
    set_logging(verbosity_level(Verbosity)),
    application:ensure_all_started(bec),
    case bitbucket_config:load(Config) of
        ok ->
            RepoConfig = proplists:get_value(repo_config, Options),
            Enforce = proplists:get_value(enforce, Options),
            K       = proplists:get_value(keep, Options),
            Delay   = proplists:get_value(delay, Options),
            case bitbucket_repo_config:verify( RepoConfig,
                                               [ {enforce, Enforce}
                                               , {delay, Delay}
                                               , {abort_on_error, not K}
                                               ]) of
                true ->
                    ok;
                false ->
                    case Enforce of
                        true -> flush_and_exit(0);
                        false -> flush_and_exit(1)
                    end
            end;
        {error, Reason} ->
            print_error_and_exit("Could not read config file ~p: (~p).~n",
                                 [ Config
                                 , Reason
                                 ])
    end.

specs() ->
    [ { help,        $h, "help",        undefined
      , "Show this help message"}
    , { config,      $c, "config",      {string, "bitbucket.config"}
      , "The BitBucket Config File"}
    , { repo_config, $r, "repo_config", {string, undefined}
      , "The Repo Config to check or configure"}
    , { delay,       $d, "delay",       {integer, 0}
      , "Delay (in seconds) between trying each repo_config"}
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
