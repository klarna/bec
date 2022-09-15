%%==============================================================================
%% A more sane Erlang API towards BitBucket
%%==============================================================================
-module(bitbucket).

-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Exports
%%==============================================================================
-export([ %% SSH Keys
          get_ssh_keys/2
        , set_ssh_keys/3
          %% Default Branch
        , get_default_branch/2
        , set_default_branch/3
          %% Workzone Branch Reviewers
        , get_wz_branch_reviewers/2
        , set_wz_branch_reviewers/3
          %% Branch Restrictions
        , get_branch_restrictions/2
        , set_branch_restrictions/3
          %% Hooks
        , get_hooks/2
        , set_hooks/3
        , disable_hook/3
        , enable_hook/3
        , get_enabled_hooks/2
        , set_enabled_hooks/3
        , get_hook_settings/3
        , set_hook_settings/4
          %% Workzone Pull Request Restrictions
        , get_wz_pr_restrictions/2
        , set_wz_pr_restrictions/3
          %% Workzone Workflow
        , get_wz_workflow/2
        , set_wz_workflow/3
        , get_push_after_pr/2
        , set_push_after_pr/3
        , get_unapprove_pr/2
        , set_unapprove_pr/3
          %% Permissions
        , get_groups/2
        , get_users/2
        , set_groups/3
        , set_users/3
          %% PR Restrictions
        , get_pr_restrictions/2
        , set_pr_restrictions/3
          %% Project
        , get_project/1
          %% Repo
        , get_repo/2
        , get_public/2
        , set_public/3
          %% Webhooks
        , get_webhooks/2
        , set_webhooks/3

          %% Setting up projects/repos/users, currently only used
          %% for testing
        , create_project/1
        , delete_project/1
        , create_repo/2
        , delete_repo/2
        , create_group/1
        , remove_group/1
        , create_user/4
        , remove_user/1
        , create_branch/4
        , add_user_to_groups/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("bitbucket.hrl").

%%==============================================================================
%% SSH Keys
%%==============================================================================
-spec get_ssh_keys(project_key(), repo_slug()) ->
        {ok, [bec_ssh_key_t:key()]} | {error, any()}.
get_ssh_keys(ProjectKey, RepoSlug) ->
  case bitbucket_api:get_ssh_keys(ProjectKey, RepoSlug) of
    {ok, Result} ->
      Values = maps:get(<<"values">>, Result),
      Keys   = [bec_ssh_key_t:from_map(V) || V <- Values],
      {ok, lists:sort(Keys)};
    {error, Reason} ->
      {error, Reason}
  end.

-spec set_ssh_keys(project_key(), repo_slug(), [bec_ssh_key_t:key()]) ->
        ok | {error, any()}.
set_ssh_keys(ProjectKey, RepoSlug, Keys) ->
  case get_ssh_keys(ProjectKey, RepoSlug) of
    {ok, OldKeys} ->
      [{ok, #{}} = bitbucket_api:delete_ssh_key(ProjectKey, RepoSlug, Id) ||
        #{id := Id} <- OldKeys],
      [{ok, #{}} = bitbucket_api:set_ssh_key( ProjectKey
                                            , RepoSlug
                                            , bec_ssh_key_t:to_map(Key))
       || Key <- Keys],
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

%%==============================================================================
%% Default Branch
%%==============================================================================
-spec get_default_branch(project_key(), repo_slug()) ->
        {ok, bec_branch_t:id()} | {error, any()}.
get_default_branch(ProjectKey, RepoSlug) ->
  case bitbucket_api:get_default_branch(ProjectKey, RepoSlug) of
    {ok, Response} when Response =:= #{} ->
      ?LOG_ERROR("Default branch not found. This might be because you haven't "
                 "pushed any commits to the repo yet."),
      throw(default_branch_not_found);
    {ok, Response} ->
      Branch = bec_branch_t:from_map(Response),
      {ok, maps:get(id, Branch)};
    {error, Reason} ->
      {error, Reason}
  end.

-spec set_default_branch( project_key()
                        , repo_slug()
                        , bec_branch_t:id()) ->
        ok | {error, any()}.
set_default_branch(ProjectKey, RepoSlug, BranchId) ->
  case bitbucket_api:set_default_branch(ProjectKey, RepoSlug,
                                        #{id => BranchId}) of
    {ok, #{}} ->
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

%%==============================================================================
%% Workzone Branch Reviewers
%%==============================================================================
-spec get_wz_branch_reviewers(project_key(), repo_slug()) ->
        {ok, [bec_wz_branch_reviewer_t:reviewer()]} | {error, any()}.
get_wz_branch_reviewers(ProjectKey, RepoSlug) ->
  case bitbucket_api:get_wz_branch_reviewers(ProjectKey, RepoSlug) of
    {ok, Values} ->
      {ok, [bec_wz_branch_reviewer_t:from_map(V) || V <- Values]};
    {error, Reason} ->
      {error, Reason}
  end.

-spec set_wz_branch_reviewers( project_key()
                             , repo_slug()
                             , [bec_wz_branch_reviewer_t:reviewer()]) ->
        ok | {error, any()}.
set_wz_branch_reviewers(ProjectKey, RepoSlug, Reviewers0) ->
  case bitbucket_api:get_wz_branch_reviewers(ProjectKey, RepoSlug) of
    {ok, OldReviewers} ->
      [{ok, _} =
         bitbucket_api:delete_wz_branch_reviewer( ProjectKey
                                                , RepoSlug
                                                , #{ <<"refName">> => RefName
                                                   }) ||
        #{ <<"refName">> := RefName } <- OldReviewers],
      Reviewers = [bec_wz_branch_reviewer_t:to_map(R) || R <- Reviewers0],
      [bitbucket_api:set_wz_branch_reviewer( ProjectKey
                                           , RepoSlug
                                           , R) || R <- Reviewers],
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

%%==============================================================================
%% Branch Restrictions
%%==============================================================================
-spec get_branch_restrictions(project_key(), repo_slug()) ->
        {ok, [bec_branch_restriction_t:restriction()]} | {error, any()}.
get_branch_restrictions(ProjectKey, RepoSlug) ->
  case bitbucket_api:get_branch_restrictions(ProjectKey, RepoSlug) of
    {ok, Response} ->
      Values = maps:get(<<"values">>, Response),
      {ok, lists:sort([bec_branch_restriction_t:from_map(V) || V <- Values])};
    {error, Reason} ->
      {error, Reason}
  end.

-spec set_branch_restrictions( project_key()
                             , repo_slug()
                             , [bec_branch_restriction_t:restriction()]) -> ok.
set_branch_restrictions(ProjectKey, RepoSlug, Restrictions0) ->
  {ok, OldRestrictions} = get_branch_restrictions(ProjectKey, RepoSlug),
  [{ok, _} = bitbucket_api:delete_branch_restriction( ProjectKey
                                                    , RepoSlug
                                                    , Id) ||
    #{ id := Id } <- OldRestrictions],
  Restrictions = [bec_branch_restriction_t:to_map(R) || R <- Restrictions0],
  [{ok, _} = bitbucket_api:set_branch_restriction( ProjectKey
                                                 , RepoSlug
                                                 , R) || R <- Restrictions],
  ok.

%%==============================================================================
%% Hooks
%%==============================================================================
-spec get_hooks(project_key(), repo_slug()) ->
        {ok, [bec_hook_t:hook()]} | {error, any()}.
get_hooks(ProjectKey, RepoSlug) ->
  case bitbucket_api:get_hooks(ProjectKey, RepoSlug) of
    {ok, Response} ->
      Values = maps:get(<<"values">>, Response),
      Hooks0 = [bec_hook_t:from_map(V) || V <- Values],
      F = fun(#{ key := Key } = Hook) ->
              {ok, Settings} = get_hook_settings(ProjectKey, RepoSlug, Key),
              Hook#{ settings => Settings }
          end,
      Hooks = lists:map(F, Hooks0),
      {ok, Hooks};
    {error, Reason} ->
      {error, Reason}
  end.

-spec set_hooks(project_key(), repo_slug(), [bec_hook_t:hook()]) ->
        ok | {error, any()}.
set_hooks(ProjectKey, RepoSlug, Hooks) ->
  [ok = set_hook_settings(ProjectKey, RepoSlug, Key, Settings) ||
    #{ key      := Key
     , settings := Settings} <- Hooks],
  Keys = [Key || #{ key := Key, enabled := true} <- Hooks],
  ok = set_enabled_hooks(ProjectKey, RepoSlug, Keys).

-spec get_enabled_hooks(project_key(), repo_slug()) ->
        {ok, [bec_hook_t:key()]} | {error, any()}.
get_enabled_hooks(ProjectKey, RepoSlug) ->
  case get_hooks(ProjectKey, RepoSlug) of
    {ok, Hooks} ->
      {ok, [Key || #{ enabled := true, key := Key } <- Hooks]};
    {error, Reason} ->
      {error, Reason}
  end.

-spec set_enabled_hooks(project_key(), repo_slug(), [bec_hook_t:key()]) ->
        ok | {error, any()}.
set_enabled_hooks(ProjectKey, RepoSlug, Keys) ->
  case get_enabled_hooks(ProjectKey, RepoSlug) of
    {ok, OldKeys} ->
      [ok = disable_hook(ProjectKey, RepoSlug, K) ||
        K <- OldKeys],
      [ok = enable_hook(ProjectKey, RepoSlug, K)|| K <- Keys],
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

-spec disable_hook(project_key(), repo_slug(), bec_hook_t:key()) ->
        ok | {error, any()}.
disable_hook(ProjectKey, RepoSlug, Key) ->
  case bitbucket_api:disable_hook(ProjectKey, RepoSlug, Key) of
    {ok, #{}} ->
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

-spec enable_hook(project_key(), repo_slug(), bec_hook_t:key()) ->
        ok | {error, any()}.
enable_hook(ProjectKey, RepoSlug, Key) ->
  case bitbucket_api:enable_hook(ProjectKey, RepoSlug, Key) of
    {ok, #{}} ->
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

-spec get_hook_settings(project_key(), repo_slug(), bec_hook_t:key()) ->
        {ok, map()} | {error, any()}.
get_hook_settings(ProjectKey, RepoSlug, Key) ->
  case bitbucket_api:get_hook_settings(ProjectKey, RepoSlug, Key) of
    {ok, Response} ->
      {ok, bec_hook_settings_t:from_map(Response)};
    {error, Reason} ->
      {error, Reason}
  end.

-spec set_hook_settings( project_key()
                       , repo_slug()
                       , bec_hook_t:key()
                       , bec_hook_settings_t:settings()) ->
        ok | {error, any()}.
set_hook_settings(ProjectKey, RepoSlug, Key, Settings) ->
  case bitbucket_api:set_hook_settings( ProjectKey
                                      , RepoSlug
                                      , Key
                                      , bec_hook_settings_t:to_map(Settings)) of
    {ok, #{}} ->
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

%%==============================================================================
%% Workzone Pull Request Restrictions
%%==============================================================================
-spec get_wz_pr_restrictions(project_key(), repo_slug()) ->
        {ok, [bec_wz_pr_restriction_t:restriction()]} | {error, any()}.
get_wz_pr_restrictions(ProjectKey, RepoSlug) ->
  case bitbucket_api:get_wz_pr_restrictions(ProjectKey, RepoSlug) of
    {ok, Values} ->
      {ok, [bec_wz_pr_restriction_t:from_map(V) || V <- Values]};
    {error, Reason} ->
      {error, Reason}
  end.

-spec set_wz_pr_restrictions( project_key()
                            , repo_slug()
                            , [bec_wz_pr_restriction_t:restriction()]) ->
        ok | {error, any()}.
set_wz_pr_restrictions(ProjectKey, RepoSlug, Restrictions) ->
  case get_wz_pr_restrictions(ProjectKey, RepoSlug) of
    {ok, OldRestrictions} ->
      [bitbucket_api:delete_wz_pr_restriction( ProjectKey
                                             , RepoSlug
                                             , bec_wz_pr_restriction_t:to_map(R)
                                             ) ||
        R <- OldRestrictions],
      [bitbucket_api:set_wz_pr_restriction( ProjectKey
                                          , RepoSlug
                                          , bec_wz_pr_restriction_t:to_map(R))
       || R <- Restrictions
      ],
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

%%==============================================================================
%% Workzone Workflow
%%==============================================================================
-spec get_wz_workflow(project_key(), repo_slug()) ->
        {ok, bec_wz_workflow_t:workflow()} | {error, any()}.
get_wz_workflow(ProjectKey, RepoSlug) ->
  case bitbucket_api:get_wz_workflow(ProjectKey, RepoSlug) of
    {ok, Response} ->
      {ok, bec_wz_workflow_t:from_map(Response)};
    {error, Reason} ->
      {error, Reason}
  end.

-spec set_wz_workflow( project_key()
                     , repo_slug()
                     , bec_wz_workflow_t:workflow()) ->
        ok | {error, any()}.
set_wz_workflow(ProjectKey, RepoSlug, Workflow0) ->
  Workflow = bec_wz_workflow_t:to_map(Workflow0),
  case bitbucket_api:set_wz_workflow(ProjectKey, RepoSlug, Workflow) of
    {ok, _} ->
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

-spec get_push_after_pr(project_key(), repo_slug()) ->
        {ok, boolean()} | {error, any()}.
get_push_after_pr(ProjectKey, RepoSlug) ->
  case get_wz_workflow(ProjectKey, RepoSlug) of
    {ok, Workflow} ->
      {ok, maps:get('push-after-pr', Workflow)};
    {error, Reason} ->
      {error, Reason}
  end.

-spec set_push_after_pr(project_key(), repo_slug(), boolean()) ->
        ok | {error, any()}.
set_push_after_pr(ProjectKey, RepoSlug, PushAfterPR) ->
  case get_wz_workflow(ProjectKey, RepoSlug) of
    {ok, Workflow} ->
      set_wz_workflow( ProjectKey
                     , RepoSlug
                     , Workflow#{ 'push-after-pr' => PushAfterPR});
    {error, Reason} ->
      {error, Reason}
  end.

-spec get_unapprove_pr(project_key(), repo_slug()) ->
        {ok, boolean()} | {error, any()}.
get_unapprove_pr(ProjectKey, RepoSlug) ->
  case get_wz_workflow(ProjectKey, RepoSlug) of
    {ok, Workflow} ->
      {ok, maps:get('unapprove-pr', Workflow)};
    {error, Reason} ->
      {error, Reason}
  end.

-spec set_unapprove_pr(project_key(), repo_slug(), boolean()) ->
        ok | {error, any()}.
set_unapprove_pr(ProjectKey, RepoSlug, PushAfterPR) ->
  case get_wz_workflow(ProjectKey, RepoSlug) of
    {ok, Workflow} ->
      set_wz_workflow( ProjectKey
                     , RepoSlug
                     , Workflow#{ 'unapprove-pr' => PushAfterPR});
    {error, Reason} ->
      {error, Reason}
  end.

%%==============================================================================
%% Permissions
%%==============================================================================
-spec get_groups(project_key(), repo_slug()) ->
        {ok, [bec_permission_group_t:permission()]} | {error, any()}.
get_groups(ProjectKey, RepoSlug) ->
  case bitbucket_api:get_permissions_groups(ProjectKey, RepoSlug) of
    {ok, Response} ->
      Values = maps:get(<<"values">>, Response),
      Permissions = [bec_permission_group_t:from_map(V) || V <- Values],
      {ok, lists:sort(Permissions)};
    {error, Reason} ->
      {error, Reason}
  end.

-spec get_users(project_key(), repo_slug()) ->
        {ok, [bec_permission_user_t:permission()]} | {error, any()}.
get_users(ProjectKey, RepoSlug) ->
  case bitbucket_api:get_permissions_users(ProjectKey, RepoSlug) of
    {ok, Response} ->
      Values = maps:get(<<"values">>, Response),
      Permissions = [bec_permission_user_t:from_map(V) || V <- Values],
      {ok, lists:sort(Permissions)};
    {error, Reason} ->
      {error, Reason}
  end.

-spec set_groups( project_key()
                , repo_slug()
                , [bec_permission_group_t:permission()]) -> ok.
set_groups(ProjectKey, RepoSlug, Permissions) ->
  {ok, OldPermissions} = get_groups(ProjectKey, RepoSlug),
  [ {ok, _} = bitbucket_api:delete_permissions_group( ProjectKey
                                                    , RepoSlug
                                                    , Groupname) ||
    #{groupname := Groupname} <- OldPermissions],
  [ {ok, _} = bitbucket_api:set_permissions_group( ProjectKey
                                                 , RepoSlug
                                                 , Permission
                                                 , Groupname) ||
    #{ permission := Permission
     , groupname  := Groupname
     } <- Permissions],
  ok.

-spec set_users( project_key()
               , repo_slug()
               , [bec_permission_user_t:permission()]) -> ok.
set_users(ProjectKey, RepoSlug, Permissions) ->
  {ok, OldPermissions} = get_users(ProjectKey, RepoSlug),
  [ {ok, _} = bitbucket_api:delete_permissions_user( ProjectKey
                                                   , RepoSlug
                                                   , Username) ||
    #{username := Username} <- OldPermissions],
  [ {ok, _} = bitbucket_api:set_permissions_user( ProjectKey
                                                , RepoSlug
                                                , Permission
                                                , Username) ||
    #{ permission := Permission
     , username   := Username
     } <- Permissions],
  ok.

%%==============================================================================
%% PR Restrictions
%%==============================================================================
-spec get_pr_restrictions(project_key(), repo_slug()) ->
        {ok, bec_pr_restriction_t:restriction()} | {error, any()}.
get_pr_restrictions(ProjectKey, RepoSlug) ->
  case bitbucket_api:get_pr_restrictions(ProjectKey, RepoSlug) of
    {ok, Response} ->
      Restrictions = bec_pr_restriction_t:from_map(Response),
      {ok, Restrictions};
    {error, Reason} ->
      {error, Reason}
  end.

-spec set_pr_restrictions( project_key()
                         , repo_slug()
                         , bec_pr_restriction_t:restriction()) ->
        ok | {error, any()}.
set_pr_restrictions(ProjectKey, RepoSlug, Restriction0) ->
  Restriction = bec_pr_restriction_t:to_map(Restriction0),
  case bitbucket_api:set_pr_restrictions( ProjectKey
                                        , RepoSlug
                                        , Restriction) of
    {ok, _} ->
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

%%==============================================================================
%% Project
%%==============================================================================
-spec get_project(project_key()) ->
        {ok, bec_project_t:project()} | {error, any()}.
get_project(ProjectKey) ->
  case bitbucket_api:get_project(ProjectKey) of
    {ok, Response} ->
      bec_project_t:from_map(Response);
    {error, Reason} ->
      {error, Reason}
  end.

%%==============================================================================
%% Repo
%%==============================================================================
-spec get_repo(project_key(), repo_slug()) ->
        {ok, bec_repo_t:repo()} | {error, any()}.
get_repo(ProjectKey, RepoSlug) ->
  case bitbucket_api:get_repo(ProjectKey, RepoSlug) of
    {ok, Response} ->
      {ok, bec_repo_t:from_map(Response)};
    {error, Reason} ->
      {error, Reason}
  end.

-spec get_public(project_key(), repo_slug()) ->
        {ok, boolean()}.
get_public(ProjectKey, RepoSlug) ->
  case get_repo(ProjectKey, RepoSlug) of
    {ok, Repo} ->
      {ok, maps:get(public, Repo)};
    {error, Reason} ->
      {error, Reason}
  end.

-spec set_public(project_key(), repo_slug(), boolean()) -> ok.
set_public(ProjectKey, RepoSlug, Public) ->
  Repo = bec_repo_t:to_map(#{public => Public}),
  case bitbucket_api:set_repo(ProjectKey, RepoSlug, Repo) of
    {ok, _} ->
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

%%==============================================================================
%% Webhooks
%%==============================================================================
-spec get_webhooks(project_key(), repo_slug()) ->
        {ok, [bec_webhook_t:webhook()]} | {error, any()}.
get_webhooks(ProjectKey, RepoSlug) ->
  case bitbucket_api:get_webhooks(ProjectKey, RepoSlug) of
    {ok, Response} ->
      Values = maps:get(<<"values">>, Response),
      {ok, lists:sort([bec_webhook_t:from_map(V) || V <- Values])};
    {error, Reason} ->
      {error, Reason}
  end.

-spec set_webhooks(project_key(), repo_slug(), [bec_webhook_t:webhook()]) -> ok.
set_webhooks(ProjectKey, RepoSlug, WebHooks) ->
  {ok, OldWebHooks} = get_webhooks(ProjectKey, RepoSlug),
  [{ok, _} = bitbucket_api:delete_webhook(ProjectKey, RepoSlug, Id) ||
    #{id := Id} <- OldWebHooks],
  [{ok, _} = bitbucket_api:set_webhook(ProjectKey, RepoSlug, WebHook) ||
    WebHook <- WebHooks],
  ok.

%%==============================================================================
%% Testing
%%==============================================================================
-spec create_project(Key :: project_key()) -> ok.
create_project(Key) ->
  {ok, _} = bitbucket_api:create_project(#{<<"key">> => Key}),
  ok.

-spec delete_project(Key :: project_key()) -> ok.
delete_project(Key) ->
  {ok, _} = bitbucket_api:delete_project(Key),
  ok.

-spec create_repo(Key :: project_key(), RepoSlug :: repo_slug()) -> ok.
create_repo(Key, RepoSlug) ->
  {ok, _} = bitbucket_api:create_repo(Key, #{<<"slug">> => RepoSlug,
                                             <<"name">> => RepoSlug,
                                             <<"scmId">> => <<"git">>}),
  ok.

-spec delete_repo(Key :: project_key(), RepoSlug :: repo_slug()) -> ok.
delete_repo(Key, RepoSlug) ->
  {ok, _} = bitbucket_api:delete_repo(Key, RepoSlug),
  ok.

-spec create_group(Group :: group()) -> ok.
create_group(Group) ->
  {ok, _} = bitbucket_api:create_group(Group),
  ok.

-spec remove_group(Group :: group()) -> ok.
remove_group(Group) ->
  {ok, _} = bitbucket_api:remove_group(Group),
  ok.

-spec create_user(UserName :: user(),
                  UserEmail :: email(),
                  UserDisplayName :: binary(),
                  Password :: binary()) ->
        ok.
create_user(UserName, UserEmail, UserDisplayName, Password) ->
  {ok, _} = bitbucket_api:create_user(UserName,
                                      UserEmail,
                                      UserDisplayName,
                                      Password),
  ok.

-spec remove_user(UserName :: user()) -> ok.
remove_user(UserName) ->
  {ok, _} = bitbucket_api:remove_user(UserName),
  ok.

-spec create_branch(ProjectKey :: project_key(),
                    RepoSlug :: repo_slug(),
                    BranchName :: branch_id(),
                    StartPoint :: branch_id()) ->
        ok.
create_branch(ProjectKey, RepoSlug, BranchName, StartPoint) ->
  {ok, _} = bitbucket_api:create_branch(ProjectKey, RepoSlug,
                                       #{<<"name">> => BranchName,
                                         <<"startPoint">> => StartPoint}),
  ok.

-spec add_user_to_groups(User :: user(),
                        Groups :: [group()]) ->
        ok.
add_user_to_groups(User, Groups) ->
  {ok, _} = bitbucket_api:add_user_to_groups(#{<<"user">> => User,
                                               <<"groups">> => Groups}),
  ok.
