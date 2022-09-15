%%==============================================================================
%% PropEr tests for the the `bec_yml` module
%%==============================================================================
-module(prop_yml).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

%%==============================================================================
%% Exports
%%==============================================================================
-compile(export_all).

%%==============================================================================
%% The property
%%==============================================================================
prop_yml() ->
  setup(),
  ?FORALL( Config
         , bec_proper_gen:config()
         , begin
             Path = "/tmp/test.yml",
             file:write_file(Path, bec_yml:encode(Config)),
             bitbucket_repo_config:verify(Path, [{enforce, true}]),
             bitbucket_repo_config:verify(Path)
           end
         ).

%%==============================================================================
%% Setup
%%==============================================================================
setup() ->
  application:load(bec),

  bec_test_utils:init_logging(),
  bec_test_utils:bitbucket_set_credentials(),

  %% For now these need to be exactly 2 each
  [TeamA, TeamB] = bec_test_utils:bitbucket_test_groups(),
  [UserA, UserB] = bec_test_utils:bitbucket_test_users(),

  application:set_env(bec, bitbucket_team_a,   TeamA),
  application:set_env(bec, bitbucket_team_b,   TeamB),
  application:set_env(bec, bitbucket_user_a,   UserA),
  application:set_env(bec, bitbucket_user_b,   UserB),
  {ok, Started} = application:ensure_all_started(bec),
  bec_test_utils:init_bitbucket(),
  #{started => Started}.
