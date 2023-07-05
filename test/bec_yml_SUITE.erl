%%==============================================================================
%% Test Suite for templates
%%==============================================================================
-module(bec_yml_SUITE).

%% Common Test Callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Testcases
-export([ environment_variable_substitution/1
        , missing_environment_variable/1
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
  yamerl_app:set_param(node_mods, [bec_node_env_variable]),
  [ {started,  Started}
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
-spec environment_variable_substitution(config()) -> ok.
environment_variable_substitution(_Config) ->
  Yml = <<"myval: !env TEST_VAL\n">>,
  try
    os:putenv("TEST_VAL", "abc"),
    ?assertEqual([#{<<"myval">> => "abc"}], bec_yml:decode(Yml)),
    os:putenv("TEST_VAL", "xyz"),
    ?assertEqual([#{<<"myval">> => "xyz"}], bec_yml:decode(Yml))
  after
    os:unsetenv("TEST_VAL")
  end.

-spec missing_environment_variable(config()) -> ok.
missing_environment_variable(_Config) ->
  Yml = <<"myval: !env TEST_VAL\n">>,
  os:unsetenv("TEST_VAL"),
  ?assertThrow({yamerl_exception, _}, bec_yml:decode(Yml)).
