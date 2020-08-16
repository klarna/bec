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
  Url           = os:getenv("BB_STAGING_URL"),
  Username      = os:getenv("BB_STAGING_USERNAME"),
  Password      = os:getenv("BB_STAGING_PASSWORD"),
  application:set_env(bec, bitbucket_url,      Url),
  application:set_env(bec, bitbucket_username, Username),
  application:set_env(bec, bitbucket_password, Password),
  {ok, Started} = application:ensure_all_started(bec),
  #{started => Started}.
