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
  Url      = os:getenv("BB_STAGING_URL", "http://localhost"),
  Username = os:getenv("BB_STAGING_USERNAME", ""),
  Password = os:getenv("BB_STAGING_PASSWORD", ""),
  TeamA    = os:getenv("BB_STAGING_TEAM_A", "team.a"),
  TeamB    = os:getenv("BB_STAGING_TEAM_B", "team.b"),
  UserA    = os:getenv("BB_STAGING_USER_A", "user.a"),
  UserB    = os:getenv("BB_STAGING_USER_B", "user.b"),
  application:set_env(bec, bitbucket_url,      Url),
  application:set_env(bec, bitbucket_username, Username),
  application:set_env(bec, bitbucket_password, Password),
  application:set_env(bec, bitbucket_team_a,   TeamA),
  application:set_env(bec, bitbucket_team_b,   TeamB),
  application:set_env(bec, bitbucket_user_a,   UserA),
  application:set_env(bec, bitbucket_user_b,   UserB),
  {ok, Started} = application:ensure_all_started(bec),
  #{started => Started}.
