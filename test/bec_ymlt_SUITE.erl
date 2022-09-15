%%==============================================================================
%% Test Suite for templates
%%==============================================================================
-module(bec_ymlt_SUITE).

%% Common Test Callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Testcases
-export([ simple_ymlt/1
        , complex_ymlt/1
        ]).

%%==============================================================================
%% Include
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type config() :: [{atom(), any()}].

%%==============================================================================
%% Common Test Callbacks
%%==============================================================================
-spec all() -> [atom()].
all() ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  {ok, Started} = application:ensure_all_started(bec),
  Url           = bec_test_utils:bitbucket_server_url(),
  Username      = bec_test_utils:bitbucket_username(),
  Password      = bec_test_utils:bitbucket_password(),
  [ {started,  Started}
  , {url,      Url}
  , {username, Username}
  , {password, Password}
    | Config
  ].

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
  [application:stop(App) || App <- ?config(started, Config)],
  ok.

-spec init_per_testcase(atom(), config()) -> any().
init_per_testcase(_TestCase, Config) ->
  Config.

-spec end_per_testcase(atom(), config()) -> any().
end_per_testcase(_TestCase, Config) ->
  Config.

%%==============================================================================
%% Test cases
%%==============================================================================
-spec simple_ymlt(config()) -> ok.
simple_ymlt(Config) ->
  Repos    = #{repo => [a, b, c]},
  Templ    = #{project => <<"test">>, repo => <<"{{repo}}">> },
  DataDir  = ?config(data_dir, Config),
  PathRepos = filename:join(DataDir, "simple_test.ymlv"),
  PathTempl = filename:join(DataDir, "simple_test.ymlt"),
  ok = file:write_file(PathRepos, bec_yml:encode(Repos)),
  ok = file:write_file(PathTempl, bec_yml:encode(Templ)),
  ?assert(bitbucket_repo_config:verify(PathTempl)).

complex_ymlt(Config) ->
  Vars     = [ #{project => test, repo => a}
             , #{project => test, repo => b}
             , #{project => test, repo => c}
             ],
  Templ    = #{project => <<"{{project}}">>, repo => <<"{{repo}}">> },
  DataDir  = ?config(data_dir, Config),
  PathRepos = filename:join(DataDir, "complex_test.ymlv"),
  PathTempl = filename:join(DataDir, "complex_test.ymlt"),
  ok = file:write_file(PathRepos, bec_yml:encode(Vars)),
  ok = file:write_file(PathTempl, bec_yml:encode(Templ)),
  ?assert(bitbucket_repo_config:verify(PathTempl)).
