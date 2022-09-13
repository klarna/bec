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

          %% For testing
        , create_project/1
        , delete_project/1
        , create_repo/2
        , delete_repo/2
        , create_group/1
        , remove_group/1
        , create_user/4
        , remove_user/1
        , create_branch/3
        , add_user_to_groups/1
        , install_plugin/2
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
%% Tests
%%==============================================================================
-spec create_project(Map :: map()) ->
        {ok, map()} | {error, map()}.
create_project(Map) ->
  Fmt   = "/rest/api/~s/projects",
  Args  = [?API_VSN],
  Url   = format_url(Fmt, Args),
  bitbucket_http:post_request(Url, jsx:encode(Map)).

-spec delete_project(Key :: project_key()) ->
        {ok, map()} | {error, map()}.
delete_project(Key) ->
  Fmt   = "/rest/api/~s/projects/~s",
  Args  = [?API_VSN, Key],
  Url   = format_url(Fmt, Args),
  bitbucket_http:delete_request(Url).

-spec create_repo(Key :: project_key(),
                  Map :: map()) ->
        {ok, map()} | {error, map()}.
create_repo(Key, Map) ->
  Fmt   = "/rest/api/~s/projects/~s/repos",
  Args  = [?API_VSN, Key],
  Url   = format_url(Fmt, Args),
  bitbucket_http:post_request(Url, jsx:encode(Map)).

-spec delete_repo(Key :: project_key(),
                  RepoSlug :: repo_slug()) ->
        {ok, map()} | {error, map()}.
delete_repo(Key, RepoSlug) ->
  Fmt   = "/rest/api/~s/projects/~s/repos/~s",
  Args  = [?API_VSN, Key, RepoSlug],
  Url   = format_url(Fmt, Args),
  bitbucket_http:delete_request(Url).

-spec create_group(Group :: group()) ->
        {ok, map()} | {error, map()}.
create_group(Group) ->
  Fmt   = "/rest/api/~s/admin/groups?name=~s",
  Args  = [?API_VSN, Group],
  Url   = format_url(Fmt, Args),
  bitbucket_http:post_request(Url, jsx:encode(#{})).

-spec remove_group(Group :: group()) ->
        {ok, map()} | {error, map()}.
remove_group(Group) ->
  Fmt   = "/rest/api/~s/admin/groups?name=~s",
  Args  = [?API_VSN, Group],
  Url   = format_url(Fmt, Args),
  bitbucket_http:delete_request(Url, jsx:encode(#{})).

-spec create_user(UserName :: user(),
                  UserEmail :: email(),
                  UserDisplayName :: binary(),
                  Password :: binary()) ->
        {ok, map()} | {error, map()}.
create_user(UserName, UserEmail, UserDisplayName, Password) ->
  Fmt   = "/rest/api/~s/admin/users?~s",
  Args  = [?API_VSN,
           uri_string:compose_query([ {"emailAddress", UserEmail}
                                    , {"displayName", UserDisplayName}
                                    , {"name", UserName}
                                    , {"password", Password}
                                    ])],
  Url   = format_url(Fmt, Args),
  bitbucket_http:post_request(Url, jsx:encode(#{})).

-spec remove_user(UserName :: user()) ->
        {ok, map()} | {error, map()}.
remove_user(UserName) ->
  Fmt   = "/rest/api/~s/admin/users?name=~s",
  Args  = [?API_VSN, UserName],
  Url   = format_url(Fmt, Args),
  bitbucket_http:delete_request(Url, jsx:encode(#{})).

-spec create_branch(ProjectKey :: project_key(),
                    RepoSlug :: repo_slug(),
                    Map :: map()) ->
        {ok, map()} | {error, map()}.
create_branch(ProjectKey, RepoSlug, Map) ->
  Fmt   = "/rest/api/~s/projects/~s/repos/~s/branches",
  Args  = [?API_VSN, ProjectKey, RepoSlug],
  Url   = format_url(Fmt, Args),
  bitbucket_http:post_request(Url, jsx:encode(Map)).

-spec add_user_to_groups(Map :: map()) ->
        {ok, map()} | {error, map()}.
add_user_to_groups(Map) ->
  Fmt   = "/rest/api/~s/admin/users/add-groups",
  Args  = [?API_VSN],
  Url   = format_url(Fmt, Args),
  bitbucket_http:post_request(Url, jsx:encode(Map)).




%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec format_url(string(), [term()]) -> string().
format_url(Fmt, Args) ->
  Url = application:get_env(bec, bitbucket_url, "http://localhost:8000"),
  lists:flatten([Url, io_lib:format(Fmt, Args)]).

%% For now
get_url(Path, Query) ->
  Username = application:get_env(bec, bitbucket_username, ""),
  Password = application:get_env(bec, bitbucket_password, ""),
  Map = uri_string:parse(
          application:get_env(bec, bitbucket_url, "http://localhost:8000")),
  uri_string:recompose(
    maps:merge(Map, #{userinfo => Username ++ ":" ++ Password,
                      path => Path,
                      query => Query})).

%% Return the UPM token to use for installing plugins. See
%% https://developer.atlassian.com/platform/marketplace/registering-apps/
-spec get_upm_token() -> binary().
get_upm_token() ->
  Url = get_url("/rest/plugins/1.0/", "os_authType=basic"),
  Headers = [{"Accept", "application/vnd.atl.plugins.installed+json"}],
  Request = {Url, Headers},
  {ok, Result} = httpc:request(get, Request, [], []),
  {{_, 200, _}, ResponseHeaders, _Body} = Result,
  list_to_binary(proplists:get_value("upm-token", ResponseHeaders)).

-spec install_plugin(PluginXmlUrl :: uri_string:uri_string(),
                     PluginName :: binary()) ->
        {ok, map()} | {error, map()}.
install_plugin(PluginXmlUrl, PluginName) ->
  UpmToken = get_upm_token(),
  Url = get_url("/rest/plugins/1.0/", "token=" ++ UpmToken),
  Headers = [{"Accept", "application/json"}],
  Body = jsx:encode(#{<<"pluginUri">> => PluginXmlUrl,
                      <<"pluginName">> => PluginName}),
  Request = {Url,
             Headers,
             "application/vnd.atl.plugins.install.uri+json",
             Body},
  %% io:format("Request: ~p~n", [Request]),
  {ok, Result} = httpc:request(post, Request, [], []),
  %% io:format("Result: ~p~n", [Result]),
  case Result of
    {{_, Code, _}, _, _} when Code >= 300 ->
      {error, Result};
    {{_, Code, _}, _, ResponseBody} when Code >= 200 ->
      io:format("~p~n", [jsx:decode(list_to_binary(ResponseBody))]),
      throw(stop_here)
  end.


  %% TODO wait for plugin to be installed
