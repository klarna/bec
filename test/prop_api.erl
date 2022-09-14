%%==============================================================================
%% PropEr tests for the the `bitbucket_api` module
%%==============================================================================
-module(prop_api).

%% Behaviour
-behaviour(proper_statem).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("proper_contrib/include/proper_contrib_statem.hrl").

%%==============================================================================
%% Exports
%%==============================================================================
-compile(export_all).

%%==============================================================================
%% Initial State
%%==============================================================================
initial_state() ->
  ProjectKey  = list_to_binary(os:getenv("BB_STAGING_PROJECT_KEY", "")),
  RepoSlug    = list_to_binary(os:getenv("BB_STAGING_REPO_SLUG", "")),
  {ok, Hooks} = bitbucket:get_hooks(ProjectKey, RepoSlug),
  #{ project_key      => ProjectKey
   , repo_slug        => RepoSlug
   , hooks            => [Key || #{key := Key} <- Hooks, is_hook_supported(Key)]
   , configured_hooks => []
   }.

%%==============================================================================
%% Commands
%%==============================================================================

%%------------------------------------------------------------------------------
%% get_default_branch/2
%%------------------------------------------------------------------------------
get_default_branch(Key, Slug) ->
  bitbucket:get_default_branch(Key, Slug).

get_default_branch_args(S) ->
  [maps:get(project_key, S), maps:get(repo_slug, S)].

get_default_branch_next(S, _R, [_Key, _Slug]) ->
  S.

get_default_branch_pre(S) ->
  maps:is_key(default_branch, S).

get_default_branch_post(S, _Args, {ok, R}) ->
  ?assertEqual(R, maps:get(default_branch, S)),
  true.

%%------------------------------------------------------------------------------
%% set_default_branch/3
%%------------------------------------------------------------------------------
set_default_branch(Key, Slug, Branch) ->
  bitbucket:set_default_branch(Key, Slug, Branch).

set_default_branch_args(S) ->
  [maps:get(project_key, S), maps:get(repo_slug, S), bec_proper_gen:branch_id()].

set_default_branch_next(S, _R, [_Key, _Slug, Branch]) ->
  maps:put(default_branch, Branch, S).

set_default_branch_post(_S, _Args, ok) ->
  true.

%%------------------------------------------------------------------------------
%% get_public/2
%%------------------------------------------------------------------------------
get_public(Key, Slug) ->
  bitbucket:get_public(Key, Slug).

get_public_args(S) ->
  [maps:get(project_key, S), maps:get(repo_slug, S)].

get_public_next(S, _R, [_Key, _Slug]) ->
  S.

get_public_pre(S) ->
  maps:is_key(public, S).

get_public_post(S, _Args, {ok, Public}) ->
  ?assertEqual(Public, maps:get(public, S)),
  true.

%%------------------------------------------------------------------------------
%% set_public/3
%%------------------------------------------------------------------------------
set_public(Key, Slug, Public) ->
  bitbucket:set_public(Key, Slug, Public).

set_public_args(S) ->
  [maps:get(project_key, S), maps:get(repo_slug, S), bool()].

set_public_next(S, _R, [_Key, _Slug, Branch]) ->
  maps:put(public, Branch, S).

set_public_post(_S, _Args, ok) ->
  true.

%%------------------------------------------------------------------------------
%% get_users/2
%%------------------------------------------------------------------------------
get_users(Key, Slug) ->
  bitbucket:get_users(Key, Slug).

get_users_args(S) ->
  [maps:get(project_key, S), maps:get(repo_slug, S)].

get_users_next(S, _R, [_Key, _Slug]) ->
  S.

get_users_pre(S) ->
  maps:is_key(users, S).

get_users_post(S, _Args, {ok, Users}) ->
  ?assertEqual(lists:sort(Users), lists:sort(maps:get(users, S))),
  true.

%%------------------------------------------------------------------------------
%% set_users/3
%%------------------------------------------------------------------------------
set_users(Key, Slug, Users) ->
  bitbucket:set_users(Key, Slug, Users).

set_users_args(S) ->
  [ maps:get(project_key, S)
  , maps:get(repo_slug, S)
  , bec_proper_gen:permission_users()
  ].

set_users_next(S, _R, [_Key, _Slug, Users]) ->
  maps:put(users, Users, S).

set_users_post(_S, _Args, ok) ->
  true.

%%------------------------------------------------------------------------------
%% get_groups/2
%%------------------------------------------------------------------------------
get_groups(Key, Slug) ->
  bitbucket:get_groups(Key, Slug).

get_groups_args(S) ->
  [maps:get(project_key, S), maps:get(repo_slug, S)].

get_groups_next(S, _R, [_Key, _Slug]) ->
  S.

get_groups_pre(S) ->
  maps:is_key(groups, S).

get_groups_post(S, _Args, {ok, Groups}) ->
  ?assertEqual(lists:sort(Groups), lists:sort(maps:get(groups, S))),
  true.

%%------------------------------------------------------------------------------
%% set_groups/3
%%------------------------------------------------------------------------------
set_groups(Key, Slug, Groups) ->
  bitbucket:set_groups(Key, Slug, Groups).

set_groups_args(S) ->
  [ maps:get(project_key, S)
  , maps:get(repo_slug, S)
  , bec_proper_gen:permission_groups()
  ].

set_groups_next(S, _R, [_Key, _Slug, Groups]) ->
  maps:put(groups, Groups, S).

set_groups_post(_S, _Args, ok) ->
  true.

%%------------------------------------------------------------------------------
%% get_hook_settings/2
%%------------------------------------------------------------------------------
get_hook_settings(Key, Slug, Hook) ->
  bitbucket:get_hook_settings(Key, Slug, Hook).

get_hook_settings_args(#{hooks := []}) ->
  true;
get_hook_settings_args(S) ->
  ?LET( {Hook, _}
      , case maps:get(configured_hooks, S) of
          %% Due to some semantic incompatibilies between EQC and
          %% PropEr, The _args function gets executed despite of the
          %% failing precondition. We therefore need to ensure that
          %% the generator does not fail.
          [] -> {<<"dummy">>, #{}};
          Hooks  -> oneof(Hooks)
        end
      , [ maps:get(project_key, S)
        , maps:get(repo_slug, S)
        , Hook
        ]).

get_hook_settings_next(S, _R, [_Key, _Slug, _Hook]) ->
  S.

get_hook_settings_pre(S) ->
  maps:get(configured_hooks, S) =/= [].

get_hook_settings_pre(S, _Args) ->
  get_hook_settings_pre(S).

get_hook_settings_post(S, [_Key, _Slug, Hook], {ok, Settings}) ->
  case proplists:get_value(Hook, maps:get(configured_hooks, S)) of
    undefined ->
      ?assertEqual(Settings, #{});
    Settings1 ->
      ?assertEqual(Settings, Settings1)
  end,
  true.

%%------------------------------------------------------------------------------
%% set_hook_settings/4
%%------------------------------------------------------------------------------
set_hook_settings(Key, Slug, Hook, Settings) ->
  bitbucket:set_hook_settings(Key, Slug, Hook, Settings).

set_hook_settings_args(#{hooks := []}) ->
  true;
set_hook_settings_args(S) ->
  ?LET( Hook
      , oneof(maps:get(hooks, S))
      , [ maps:get(project_key, S)
        , maps:get(repo_slug, S)
        , Hook
        , bec_proper_gen:hook_settings(Hook)
        ]).

set_hook_settings_pre(S) ->
  maps:get(configured_hooks, S) =/= [].

set_hook_settings_pre(S, _Args) ->
  set_hook_settings_pre(S).

set_hook_settings_next(S, _R, [_Key, _Slug, Hook, Settings]) ->
  Hooks0 = maps:get(configured_hooks, S),
  Hooks  = case lists:keyfind(Hook, 1, Hooks0) of
             {Hook, _}   ->
               lists:keyreplace(Hook, 1, Hooks0, {Hook, Settings});
             false ->
               %% Appending to improve shrinking
               Hooks0 ++ [{Hook, Settings}]
           end,
  S#{ configured_hooks => Hooks }.

set_hook_settings_post(_S, _Args, ok) ->
  true.

%%------------------------------------------------------------------------------
%% get_enabled_hooks/2
%%------------------------------------------------------------------------------
get_enabled_hooks(Key, Slug) ->
  bitbucket:get_enabled_hooks(Key, Slug).

get_enabled_hooks_args(S) ->
  [maps:get(project_key, S), maps:get(repo_slug, S)].

get_enabled_hooks_next(S, _R, [_Key, _Slug]) ->
  S.

get_enabled_hooks_pre(S) ->
  maps:is_key(enabled_hooks, S).

get_enabled_hooks_post(S, _Args, {ok, Hooks}) ->
  ?assertEqual(lists:sort(Hooks), lists:sort(maps:get(enabled_hooks, S))),
  true.

%%------------------------------------------------------------------------------
%% set_enabled_hooks/3
%%------------------------------------------------------------------------------
set_enabled_hooks(Key, Slug, EnabledHooks) ->
  bitbucket:set_enabled_hooks(Key, Slug, EnabledHooks).

set_enabled_hooks_args(S) ->
  ?LET( Hooks
      , maps:get(configured_hooks, S)
      , [ maps:get(project_key, S)
        , maps:get(repo_slug, S)
        , bec_proper_gen:sublist([ H || {H, _} <- Hooks])
        ]).

set_enabled_hooks_next(S, _R, [_Key, _Slug, Hooks]) ->
  maps:put(enabled_hooks, Hooks, S).

set_enabled_hooks_post(_S, _Args, ok) ->
  true.

%%------------------------------------------------------------------------------
%% get_branch_restrictions/2
%%------------------------------------------------------------------------------
get_branch_restrictions(Key, Slug) ->
  bitbucket:get_branch_restrictions(Key, Slug).

get_branch_restrictions_args(S) ->
  [maps:get(project_key, S), maps:get(repo_slug, S)].

get_branch_restrictions_next(S, _R, [_Key, _Slug]) ->
  S.

get_branch_restrictions_pre(S) ->
  maps:is_key(branch_restrictions, S).

get_branch_restrictions_post(S, _Args, {ok, Restrictions0}) ->
  Restrictions = [maps:remove(id, R)|| R <- Restrictions0],
  ?assertEqual(lists:sort(Restrictions), lists:sort(maps:get(branch_restrictions, S))),
  true.

%%------------------------------------------------------------------------------
%% set_branch_restrictions/3
%%------------------------------------------------------------------------------
set_branch_restrictions(Key, Slug, Restrictions) ->
  bitbucket:set_branch_restrictions(Key, Slug, Restrictions).

set_branch_restrictions_args(S) ->
  [ maps:get(project_key, S)
  , maps:get(repo_slug, S)
  , bec_proper_gen:branch_restrictions()
  ].

set_branch_restrictions_next(S, _R, [_Key, _Slug, Restrictions]) ->
  maps:put(branch_restrictions, Restrictions, S).

set_branch_restrictions_post(_S, _Args, ok) ->
  true.

%%------------------------------------------------------------------------------
%% get_wz_branch_reviewers/2
%%------------------------------------------------------------------------------
get_wz_branch_reviewers(Key, Slug) ->
  bitbucket:get_wz_branch_reviewers(Key, Slug).

get_wz_branch_reviewers_args(#{is_wz_supported := false}) ->
  true;
get_wz_branch_reviewers_args(S) ->
  [maps:get(project_key, S), maps:get(repo_slug, S)].

get_wz_branch_reviewers_next(S, _R, [_Key, _Slug]) ->
  S.

get_wz_branch_reviewers_pre(S) ->
  maps:is_key(wz_branch_reviewers, S) andalso
    maps:get(is_wz_supported, S, false).

get_wz_branch_reviewers_post(S, _Args, {ok, Reviewers}) ->
  ?assertEqual(lists:sort(Reviewers), lists:sort(maps:get(wz_branch_reviewers, S))),
  true.

%%------------------------------------------------------------------------------
%% set_wz_branch_reviewers/3
%%------------------------------------------------------------------------------
set_wz_branch_reviewers(Key, Slug, Reviewers) ->
  bitbucket:set_wz_branch_reviewers(Key, Slug, Reviewers).

set_wz_branch_reviewers_args(#{is_wz_supported := false}) ->
  true;
set_wz_branch_reviewers_args(S) ->
  [ maps:get(project_key, S)
  , maps:get(repo_slug, S)
  , bec_proper_gen:wz_branch_reviewers()
  ].

set_wz_branch_reviewers_pre(S) ->
  maps:get(is_wz_supported, S, false).

set_wz_branch_reviewers_next(S, _R, [_Key, _Slug, Reviewers]) ->
  maps:put(wz_branch_reviewers, Reviewers, S).

set_wz_branch_reviewers_post(_S, _Args, ok) ->
  true.

%%------------------------------------------------------------------------------
%% get_webhooks/2
%%------------------------------------------------------------------------------
get_webhooks(Key, Slug) ->
  bitbucket:get_webhooks(Key, Slug).

get_webhooks_args(S) ->
  [maps:get(project_key, S), maps:get(repo_slug, S)].

get_webhooks_next(S, _R, [_Key, _Slug]) ->
  S.

get_webhooks_pre(S) ->
  maps:is_key(webhooks, S).

get_webhooks_post(S, _Args, {ok, WebHooks0}) ->
  Generated = [id, createdDate, updatedDate],
  Webhooks = [lists:foldl(fun maps:remove/2, WH, Generated)|| WH <- WebHooks0],
  ?assertEqual(lists:sort(Webhooks), lists:sort(maps:get(webhooks, S))),
  true.

%%------------------------------------------------------------------------------
%% set_webhooks/3
%%------------------------------------------------------------------------------
set_webhooks(Key, Slug, WebHooks) ->
  bitbucket:set_webhooks(Key, Slug, WebHooks).

set_webhooks_args(S) ->
  [ maps:get(project_key, S)
  , maps:get(repo_slug, S)
  , bec_proper_gen:webhooks()
  ].

set_webhooks_next(S, _R, [_Key, _Slug, WebHooks]) ->
  maps:put(webhooks, WebHooks, S).

set_webhooks_post(_S, _Args, ok) ->
  true.

%%==============================================================================
%% The statem's property
%%==============================================================================
prop_api() ->
  proper_contrib_statem:run(?MODULE, #{ setup_fun    => fun setup/0
                                      , teardown_fun => fun teardown/1
                                      }).

%%==============================================================================
%% Setup
%%==============================================================================

setup() ->
  bec_test_utils:init_logging(),
  application:load(bec),
  Url           = os:getenv("BB_STAGING_URL", "http://localhost"),
  Username      = os:getenv("BB_STAGING_USERNAME", ""),
  Password      = os:getenv("BB_STAGING_PASSWORD", ""),
  application:set_env(bec, bitbucket_url,      Url),
  application:set_env(bec, bitbucket_username, Username),
  application:set_env(bec, bitbucket_password, Password),
  {ok, Started} = application:ensure_all_started(bec),
  bec_test_utils:init_bitbucket(),
  #{started => Started,
    wz_supported => bec_test_utils:is_wz_supported()}.


%%==============================================================================
%% Teardown
%%==============================================================================
teardown(#{started := Started}) ->
  bec_test_utils:deinit_bitbucket(),
  [application:stop(App) || App <- Started],
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% Return true for those hooks which are supported by BEC.
is_hook_supported(<<"com.nerdwin15.stash-stash-webhook-jenkins:jenkinsPostReceiveHook">>) ->
  true;
is_hook_supported(<<"de.aeffle.stash.plugin.stash-http-get-post-receive-hook:http-get-post-receive-hook">>) ->
  true;
is_hook_supported(_Key) ->
  false.
