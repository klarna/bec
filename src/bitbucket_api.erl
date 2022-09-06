%%==============================================================================
%% An Erlang wrapper around the BitBucket HTTP API
%%==============================================================================
-module(bitbucket_api).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ %% SSH Keys
          delete_ssh_key/3
        , get_ssh_keys/2
        , set_ssh_key/3
          %% Default Branch
        , get_default_branch/2
        , set_default_branch/3
          %% Workzone Branch Reviewer
        , delete_wz_branch_reviewer/3
        , get_wz_branch_reviewers/2
        , set_wz_branch_reviewer/3
          %% Branch Restrictions
        , delete_branch_restriction/3
        , get_branch_restrictions/2
        , set_branch_restriction/3
          %% Hooks
        , disable_hook/3
        , enable_hook/3
        , get_hooks/2
        , get_hook_settings/3
        , set_hook_settings/4
          %% Workzone Pull Request Restrictions
        , delete_wz_pr_restriction/3
        , get_wz_pr_restrictions/2
        , set_wz_pr_restriction/3
          %% Workzone Workflow
        , get_wz_workflow/2
        , set_wz_workflow/3
          %% Permissions
        , delete_permissions_group/3
        , delete_permissions_user/3
        , get_permissions_groups/2
        , get_permissions_users/2
        , set_permissions_group/4
        , set_permissions_user/4
          %% PR Restrictions
        , get_pr_restrictions/2
        , set_pr_restrictions/3
          %% Project
        , get_project/1
          %% Repo
        , get_repo/2
        , set_repo/3
          %% Webhooks
        , delete_webhook/3
        , get_webhooks/2
        , set_webhook/3
        ]).

-include("bitbucket.hrl").

-define(API_VSN, "1.0").
-define(BRANCH_PERMISSIONS_API_VSN, "2.0").
-define(WORKZONE_API_VSN, "1.0").

%%==============================================================================
%% SSH Keys
%%==============================================================================
-spec delete_ssh_key(project_key(), repo_slug(), bec_ssh_key_t:id()) ->
        {ok, map()} | {error, any()}.
delete_ssh_key(ProjectKey, RepoSlug, Id) ->
  Fmt  = "/rest/keys/~s/projects/~s/repos/~s/ssh/~p",
  Args = [?API_VSN, ProjectKey, RepoSlug, Id],
  Url  = format_url(Fmt, Args),
  bitbucket_http:delete_request(Url).

-spec get_ssh_keys(project_key(), repo_slug()) ->
        {ok, map()} | {error, any()}.
get_ssh_keys(ProjectKey, RepoSlug) ->
  Fmt  = "/rest/keys/~s/projects/~s/repos/~s/ssh",
  Args = [?API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  bitbucket_http:get_request(Url).

-spec set_ssh_key(project_key(), repo_slug(), map()) ->
        {ok, map()} | {error, any()}.
set_ssh_key(ProjectKey, RepoSlug, Body) ->
  Fmt  = "/rest/keys/~s/projects/~s/repos/~s/ssh",
  Args = [?API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  bitbucket_http:post_request(Url, jsx:encode(Body)).

%%==============================================================================
%% Default Branch
%%==============================================================================
-spec get_default_branch(project_key(), repo_slug()) ->
        {ok, map()} | {error, any()}.
get_default_branch(ProjectKey, RepoSlug) ->
  Fmt  = "/rest/api/~s/projects/~s/repos/~s/branches/default",
  Args = [?API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  bitbucket_http:get_request(Url).

-spec set_default_branch(project_key(), repo_slug(), map()) ->
        {ok, map()} | {error, any()}.
set_default_branch(ProjectKey, RepoSlug, Body) ->
  Fmt  = "/rest/api/~s/projects/~s/repos/~s/branches/default",
  Args = [?API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  bitbucket_http:put_request(Url, jsx:encode(Body)).

%%==============================================================================
%% Workzone Branch Reviewers
%%==============================================================================
-spec delete_wz_branch_reviewer(project_key(), repo_slug(), map()) ->
        {ok, map()} | {error, any()}.
delete_wz_branch_reviewer(ProjectKey, RepoSlug, Map) ->
  Fmt  = "/rest/workzoneresource/~s/branch/reviewers/~s/~s",
  Args = [?WORKZONE_API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  bitbucket_http:delete_request(Url, jsx:encode(Map)).

-spec get_wz_branch_reviewers(project_key(), repo_slug()) ->
        {ok, [map()]} | {error, any()}.
get_wz_branch_reviewers(ProjectKey, RepoSlug) ->
  Fmt  = "/rest/workzoneresource/~s/branch/reviewers/~s/~s",
  Args = [?WORKZONE_API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  case bitbucket_http:get_request(Url) of
    %% 'No Content' is converted into an empty map, but we need [] instead:
    {ok, #{} = M} when map_size(M) == 0 -> {ok, []};
    Other -> Other
  end.

-spec set_wz_branch_reviewer(project_key(), repo_slug(), map()) ->
        {ok, map()} | {error, any()}.
set_wz_branch_reviewer(ProjectKey, RepoSlug, Map) ->
  Fmt    = "/rest/workzoneresource/~s/branch/reviewers/~s/~s",
  Args   = [?WORKZONE_API_VSN, ProjectKey, RepoSlug],
  Url    = format_url(Fmt, Args),
  bitbucket_http:post_request(Url, jsx:encode(Map)).

%%==============================================================================
%% Branch Restrictions
%%==============================================================================
-spec delete_branch_restriction( project_key()
                               , repo_slug()
                               , bec_branch_restriction_t:id()) ->
        {ok, map()} | {error, any()}.
delete_branch_restriction(ProjectKey, RepoSlug, Id) ->
  Fmt  = "/rest/branch-permissions/~s/projects/~s/repos/~s/restrictions/~p",
  Args = [?BRANCH_PERMISSIONS_API_VSN, ProjectKey, RepoSlug, Id],
  Url   = format_url(Fmt, Args),
  bitbucket_http:delete_request(Url).

-spec get_branch_restrictions(project_key(), repo_slug()) ->
        {ok, map()} | {error, any()}.
get_branch_restrictions(ProjectKey, RepoSlug) ->
  Fmt  = "/rest/branch-permissions/~s/projects/~s/repos/~s/restrictions",
  Args = [?BRANCH_PERMISSIONS_API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  bitbucket_http:get_request(Url).

-spec set_branch_restriction(project_key(), repo_slug(), map()) ->
        {ok, map()} | {error, any()}.
set_branch_restriction(ProjectKey, RepoSlug, Map) ->
  Fmt   = "/rest/branch-permissions/~s/projects/~s/repos/~s/restrictions",
  Args  = [?BRANCH_PERMISSIONS_API_VSN, ProjectKey, RepoSlug],
  Url   = format_url(Fmt, Args),
  bitbucket_http:post_request(Url, jsx:encode(Map)).

%%==============================================================================
%% Hooks
%%==============================================================================
-spec disable_hook(project_key(), repo_slug(), bec_hook_t:key()) ->
        {ok, map()} | {error, any()}.
disable_hook(ProjectKey, RepoSlug, Key) ->
  Fmt  = "/rest/api/~s/projects/~s/repos/~s/settings/hooks/~s/enabled",
  Args = [?API_VSN, ProjectKey, RepoSlug, Key],
  Url  = format_url(Fmt, Args),
  bitbucket_http:delete_request(Url).

-spec enable_hook(project_key(), repo_slug(), bec_hook_t:key()) ->
        {ok, map()} | {error, any()}.
enable_hook(ProjectKey, RepoSlug, Key) ->
  Fmt  = "/rest/api/~s/projects/~s/repos/~s/settings/hooks/~s/enabled",
  Args = [?API_VSN, ProjectKey, RepoSlug, Key],
  Url  = format_url(Fmt, Args),
  bitbucket_http:put_request(Url, <<>>).

-spec get_hooks(project_key(), repo_slug()) ->
        {ok, map()} | {error, any()}.
get_hooks(ProjectKey, RepoSlug) ->
  Fmt  = "/rest/api/~s/projects/~s/repos/~s/settings/hooks",
  Args = [?API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  bitbucket_http:get_request(Url).

-spec get_hook_settings(project_key(), repo_slug(), bec_hook_t:key()) ->
        {ok, map()} | {error, any()}.
get_hook_settings(ProjectKey, RepoSlug, Key) ->
  Fmt  = "/rest/api/~s/projects/~s/repos/~s/settings/hooks/~s/settings",
  Args = [?API_VSN, ProjectKey, RepoSlug, Key],
  Url  = format_url(Fmt, Args),
  bitbucket_http:get_request(Url).

-spec set_hook_settings(project_key(), repo_slug(), bec_hook_t:key(), map()) ->
        {ok, map()} | {error, any()}.
set_hook_settings(ProjectKey, RepoSlug, Key, Map) ->
  Fmt  = "/rest/api/~s/projects/~s/repos/~s/settings/hooks/~s/settings",
  Args = [?API_VSN, ProjectKey, RepoSlug, Key],
  Url  = format_url(Fmt, Args),
  bitbucket_http:put_request(Url, jsx:encode(Map)).

%%==============================================================================
%% Workzone Pull Request Restrictions
%%==============================================================================
-spec delete_wz_pr_restriction(project_key(), repo_slug(), map()) ->
        {ok, map()} | {error, any()}.
delete_wz_pr_restriction(ProjectKey, RepoSlug, Map) ->
  Fmt  = "/rest/workzoneresource/~s/branch/automerge/~s/~s",
  Args = [?WORKZONE_API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  bitbucket_http:delete_request(Url, jsx:encode(Map)).

-spec get_wz_pr_restrictions(project_key(), repo_slug()) ->
        {ok, [map()]} | {error, any()}.
get_wz_pr_restrictions(ProjectKey, RepoSlug) ->
  Fmt  = "/rest/workzoneresource/~s/branch/automerge/~s/~s",
  Args = [?WORKZONE_API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  %% New versions of the Workzone plugin return an empty body when
  %% there are no restrictions set instead of an empty list, so we
  %% need to handle both cases.
  case bitbucket_http:get_request(Url) of
    {ok, #{}} ->
      {ok, []};
    {ok, List} when is_list(List) ->
      {ok, List};
    {error, Error} ->
      {error, Error}
  end.

-spec set_wz_pr_restriction(project_key(), repo_slug(), map()) ->
        {ok, map()} | {error, any()}.
set_wz_pr_restriction(ProjectKey, RepoSlug, Map) ->
  Fmt    = "/rest/workzoneresource/~s/branch/automerge/~s/~s",
  Args   = [?WORKZONE_API_VSN, ProjectKey, RepoSlug],
  Url    = format_url(Fmt, Args),
  bitbucket_http:post_request(Url, jsx:encode(Map)).

%%==============================================================================
%% Workzone Workflow
%%==============================================================================
-spec get_wz_workflow(project_key(), repo_slug()) ->
       {ok, map()} | {error, any()}.
get_wz_workflow(ProjectKey, RepoSlug) ->
  Fmt  = "/rest/workzoneresource/~s/workflow/~s/~s",
  Args = [?WORKZONE_API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  bitbucket_http:get_request(Url).

-spec set_wz_workflow(project_key(), repo_slug(), map()) ->
        {ok, map()} | {error, any()}.
set_wz_workflow(ProjectKey, RepoSlug, Map) ->
  Fmt  = "/rest/workzoneresource/~s/workflow/~s/~s",
  Args = [?WORKZONE_API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  bitbucket_http:post_request(Url, jsx:encode(Map)).

%%==============================================================================
%% Permissions
%%==============================================================================
-spec delete_permissions_group( project_key()
                              , repo_slug()
                              , bec_group_t:name()) ->
        {ok, map()} | {error, any()}.
delete_permissions_group(ProjectKey, RepoSlug, GroupName) ->
  Fmt   = "/rest/api/~s/projects/~s/repos/~s/permissions/groups",
  Query = "?name=~s",
  Args  = [?API_VSN, ProjectKey, RepoSlug, GroupName],
  Url   = format_url(Fmt ++ Query, Args),
  bitbucket_http:delete_request(Url).

-spec delete_permissions_user( project_key()
                             , repo_slug()
                             , bec_user_t:name()) ->
        {ok, map()} | {error, any()}.
delete_permissions_user(ProjectKey, RepoSlug, Username) ->
  Fmt   = "/rest/api/~s/projects/~s/repos/~s/permissions/users",
  Query = "?name=~s",
  Args  = [?API_VSN, ProjectKey, RepoSlug, Username],
  Url   = format_url(Fmt ++ Query, Args),
  bitbucket_http:delete_request(Url).

-spec get_permissions_groups(project_key(), repo_slug()) ->
        {ok, map()} | {error, any()}.
get_permissions_groups(ProjectKey, RepoSlug) ->
  Fmt  = "/rest/api/~s/projects/~s/repos/~s/permissions/groups",
  Args = [?API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  bitbucket_http:get_request(Url).

-spec get_permissions_users(project_key(), repo_slug()) ->
        {ok, map()} | {error, any()}.
get_permissions_users(ProjectKey, RepoSlug) ->
  Fmt  = "/rest/api/~s/projects/~s/repos/~s/permissions/users",
  Args = [?API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  bitbucket_http:get_request(Url).

-spec set_permissions_group( project_key()
                           , repo_slug()
                           , bec_permission_group_t:permission_type()
                           , bec_group_t:name()
                           ) -> {ok, map()} | {error, any()}.
set_permissions_group(ProjectKey, RepoSlug, Permission, Groupname) ->
  Fmt   = "/rest/api/~s/projects/~s/repos/~s/permissions/groups",
  Query = "?permission=~s&name=~s",
  Args  = [?API_VSN, ProjectKey, RepoSlug, Permission, Groupname],
  Url   = format_url(Fmt ++ Query, Args),
  bitbucket_http:put_request(Url, <<>>).

-spec set_permissions_user( project_key()
                          , repo_slug()
                          , bec_permission_user_t:permission_type()
                          , bec_user_t:name()
                          ) -> {ok, map()} | {error, any()}.
set_permissions_user(ProjectKey, RepoSlug, Permission, Username) ->
  Fmt   = "/rest/api/~s/projects/~s/repos/~s/permissions/users",
  Query = "?permission=~s&name=~s",
  Args  = [?API_VSN, ProjectKey, RepoSlug, Permission, Username],
  Url   = format_url(Fmt ++ Query, Args),
  bitbucket_http:put_request(Url, <<>>).

%%==============================================================================
%% PR Restrictions
%%==============================================================================
-spec get_pr_restrictions(project_key(), repo_slug()) ->
        {ok, map()} | {error, any()}.
get_pr_restrictions(ProjectKey, RepoSlug) ->
  Fmt  = "/rest/api/~s/projects/~s/repos/~s/settings/pull-requests",
  Args = [?API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  bitbucket_http:get_request(Url).

-spec set_pr_restrictions(project_key(), repo_slug(), map()) ->
        {ok, map()} | {error, any()}.
set_pr_restrictions(ProjectKey, RepoSlug, Map) ->
  Fmt  = "/rest/api/~s/projects/~s/repos/~s/settings/pull-requests",
  Args = [?API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  bitbucket_http:post_request(Url, jsx:encode(Map)).

%%==============================================================================
%% Project
%%==============================================================================
-spec get_project(project_key()) -> {ok, map()} | {error, any()}.
get_project(ProjectKey) ->
  Fmt  = "/rest/api/~s/projects/~s",
  Args = [?API_VSN, ProjectKey],
  Url  = format_url(Fmt, Args),
  bitbucket_http:get_request(Url).

%%==============================================================================
%% Repo
%%==============================================================================
-spec get_repo(project_key(), repo_slug()) -> {ok, map()} | {error, any()}.
get_repo(ProjectKey, RepoSlug) ->
  Fmt  = "/rest/api/~s/projects/~s/repos/~s",
  Args = [?API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  bitbucket_http:get_request(Url).

-spec set_repo(project_key(), repo_slug(), map()) ->
        {ok, map()} | {error, any()}.
set_repo(ProjectKey, RepoSlug, Map) ->
  Fmt  = "/rest/api/~s/projects/~s/repos/~s",
  Args = [?API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  bitbucket_http:put_request(Url, jsx:encode(Map)).

%%==============================================================================
%% Webhooks
%%==============================================================================
-spec delete_webhook( project_key()
                    , repo_slug()
                    , bec_webhook_t:id()) ->
        {ok, map()} | {error, any()}.
delete_webhook(ProjectKey, RepoSlug, Id) ->
  Fmt  = "/rest/api/~s/projects/~s/repos/~s/webhooks/~p",
  Args = [?API_VSN, ProjectKey, RepoSlug, Id],
  Url   = format_url(Fmt, Args),
  bitbucket_http:delete_request(Url).

-spec get_webhooks(project_key(), repo_slug()) -> {ok, map()} | {error, any()}.
get_webhooks(ProjectKey, RepoSlug) ->
  Fmt  = "/rest/api/~s/projects/~s/repos/~s/webhooks",
  Args = [?API_VSN, ProjectKey, RepoSlug],
  Url  = format_url(Fmt, Args),
  bitbucket_http:get_request(Url).

-spec set_webhook(project_key(), repo_slug(), map()) ->
        {ok, map()} | {error, any()}.
set_webhook(ProjectKey, RepoSlug, Map) ->
  Fmt  = "/rest/api/~s/projects/~s/repos/~s/webhooks",
  Args  = [?API_VSN, ProjectKey, RepoSlug],
  Url   = format_url(Fmt, Args),
  bitbucket_http:post_request(Url, jsx:encode(Map)).

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec format_url(string(), [term()]) -> string().
format_url(Fmt, Args) ->
  Url = application:get_env(bec, bitbucket_url, "http://localhost:8000"),
  lists:flatten([Url, io_lib:format(Fmt, Args)]).
