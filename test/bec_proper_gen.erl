%%==============================================================================
%% A collection of PropEr generators for the BitBucket types
%%==============================================================================
-module(bec_proper_gen).

-compile(export_all).

-include_lib("proper/include/proper.hrl").

%%==============================================================================
%% Branch
%%==============================================================================
branch_id() ->
  oneof([<<"master">>, <<"feature">>]).

branch() ->
  ?LET(Id, branch_id(), #{id =>  Id}).

%%==============================================================================
%% User
%%==============================================================================
username() ->
  oneof([user_a(), user_b()]).

usernames() ->
  unique_list(username()).

%%==============================================================================
%% Group
%%==============================================================================
groupname() ->
  oneof([team_a(), team_b()]).

group() ->
  ?LET(GroupName, groupname(), #{groupname => GroupName}).

groupnames() ->
  unique_list(groupname()).

groups() ->
  unique_list(group(), {map, groupname}).

%%==============================================================================
%% Mandatory attributes
%%==============================================================================
mandatory_users() ->
  unique_list(username()).

mandatory_groups() ->
  unique_list(groupname()).

%%==============================================================================
%% Permission User
%%==============================================================================
permission_type() ->
  oneof(['REPO_ADMIN', 'REPO_WRITE', 'REPO_READ']).

permission_user() ->
  ?LET( {Permission, UserName}
      , {permission_type(), username()}
      , #{ permission  => Permission
         , username    => UserName
         }).

permission_users() ->
  unique_list(permission_user(), {map, username}).

ss_permission_users() ->
  ?LET( Users
      , non_empty(permission_users())
      , bec_config_ss:from_permission_users(Users)
      ).

%%==============================================================================
%% Permission Group
%%==============================================================================
permission_group() ->
  ?LET( {Permission, GroupName}
      , {permission_type(), groupname()}
      , #{ permission  => Permission
         , groupname   => GroupName
         }).

permission_groups() ->
  unique_list(permission_group(), {map, groupname}).

ss_permission_groups() ->
  ?LET( Groups
      , non_empty(permission_groups())
      , bec_config_ss:from_permission_groups(Groups)
      ).

%%==============================================================================
%% Hooks
%%==============================================================================
hook_settings(<<"com.nerdwin15.stash-stash-webhook-jenkins:jenkinsPostReceiveHook">>) ->
  ?LET( {JenkinsBase, IgnoreCerts, OmitBranchName, OmitHashCode, OmitTriggerBuildButton}
      , {uri(), bool(), bool(), bool(), bool()}
      , #{ <<"jenkinsBase">>            => JenkinsBase
         , <<"cloneType">>              => <<"ssh">>
         , <<"ignoreCerts">>            => IgnoreCerts
         , <<"omitBranchName">>         => OmitBranchName
         , <<"omitHashCode">>           => OmitHashCode
         , <<"omitTriggerBuildButton">> => OmitTriggerBuildButton
         });
hook_settings(<<"de.aeffle.stash.plugin.stash-http-get-post-receive-hook:http-get-post-receive-hook">>) ->
  ?LET({Url, Url2}, {uri(), uri()}, #{ <<"url">>  => Url, <<"url2">> => Url2});
hook_settings(Hook) when is_binary(Hook) ->
  #{}.

hook_id() ->
  oneof([ <<"com.nerdwin15.stash-stash-webhook-jenkins:jenkinsPostReceiveHook">>
        , <<"de.aeffle.stash.plugin.stash-http-get-post-receive-hook:http-get-post-receive-hook">>
        ]).

hook_ids() ->
  unique_list(hook_id()).

hooks() ->
   ?LET( Ids
       , non_empty(hook_ids())
       , ?LET( Hooks
             , [{bool(), I, hook_settings(I)} || I <- Ids]
             , [#{ <<"enabled">>  => Enabled
                 , <<"key">>      => Key
                 , <<"settings">> => Settings
                 } || {Enabled, Key, Settings} <- Hooks])).

%%==============================================================================
%% Branch Restriction
%%==============================================================================
matcher_type() ->
  oneof([ 'BRANCH', 'PATTERN' ]).

restriction_type() ->
  oneof([ 'fast-forward-only'
        , 'no-deletes'
        , 'pull-request-only'
        , 'read-only'
        ]).

branch_restriction() ->
  ?LET( {BranchId, MatcherType, Type, Users, Groups}
      , {branch_id(), matcher_type(), restriction_type(), usernames(), groupnames()}
      , #{ 'branch-id'    => BranchId
         , 'matcher-type' => MatcherType
         , type           => Type
         , users          => Users
         , groups         => Groups
         }).

branch_restrictions() ->
  unique_list(branch_restriction(), {map, 'branch-id'}).

ss_branch_restrictions() ->
  ?LET( Restrictions
      , non_empty(branch_restrictions())
      , bec_config_ss:from_branch_restrictions(Restrictions)
      ).

%%==============================================================================
%% Workzone Workflow
%%==============================================================================
wz_workflow() ->
  ?LET( {PushAfterPr, UnapprovePR}
      , {bool(), bool()}
      , #{ 'push-after-pr' => PushAfterPr
         , 'unapprove-pr'  => UnapprovePR
         }).

%%==============================================================================
%% Workzone PR Restriction
%%==============================================================================
wz_pr_restrictions() ->
  unique_list(wz_pr_restriction(), {map, 'branch-id'}).

wz_pr_restriction() ->
  ?LET( {BranchId, ApprovalQuota, GroupQuota}
      , {branch_id(), non_zero_nat(), non_zero_nat()}
      , #{ 'branch-id'      => BranchId
         , 'approval-quota' => ApprovalQuota
         , 'group-quota'    => GroupQuota
         }).

%%==============================================================================
%% Workzone Branch Reviewer
%%==============================================================================
wz_branch_reviewers() ->
  unique_list(wz_branch_reviewer(), {map, 'branch-id'}).

wz_branch_reviewer() ->
  ?LET( {BranchId, Users, Groups, Paths, MandatoryUsers, MandatoryGroups}
      , {branch_id(), usernames(), groupnames(), unique_list(wz_path()), mandatory_users(), mandatory_groups()}
      , #{ 'branch-id'        => BranchId
         , users              => Users
         , groups             => Groups
         , paths              => Paths
         , 'mandatory-users'  => MandatoryUsers
         , 'mandatory-groups' => MandatoryGroups
         }).

wz_path() ->
  ?LET( {Groups, Users, Path, MandatoryUsers, MandatoryGroups}
      , {groupnames(), usernames(), path(), mandatory_users(), mandatory_groups()}
      , #{ groups => Groups
         , users  => Users
         , path   => Path
         , 'mandatory-users'  => MandatoryUsers
         , 'mandatory-groups' => MandatoryGroups
         }).

path() ->
  non_empty(string_b()).

%%==============================================================================
%% PR Restrictions
%%==============================================================================
pr_restrictions() ->
  ?LET( {MergeConfig, AllApprovers, AllTasks, Approvers, Builds}
      , {merge_config(), bool(), bool(), nat(), nat()}
      , #{ 'merge-config'                => MergeConfig
         , 'required-all-approvers'      => AllApprovers
         , 'required-all-tasks-complete' => AllTasks
         , 'required-approvers'          => Approvers
         , 'required-successful-builds'  => Builds
         }).

merge_config() ->
  ?LET( Default
      , merge_strategy_type()
      , ?LET( Strategies
            , merge_strategies(Default)
            , #{ 'default-strategy'   => Default
               , 'enabled-strategies' => Strategies
               })).

merge_strategies(Default) ->
  ?SUCHTHAT( Strategies
           , unique_list(merge_strategy_type())
           , lists:member(Default, Strategies)
           ).

merge_strategy() ->
  ?LET( {Id, Enabled}
      , {merge_strategy_type(), bool()}
      , #{ id      => Id
         , enabled => Enabled
         }).

merge_strategy_type() ->
  oneof([ 'ff'
        , 'ff-only'
        , 'no-ff'
        , 'squash'
        , 'squash-ff-only'
        , 'rebase-no-ff'
        , 'rebase-ff-only'
        ]).

%%==============================================================================
%% SSH Key
%%==============================================================================
ssh_key() ->
  oneof([ <<"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCegj3blYg/6IPdm16Fo1DhgDLPtDgyzvgKPC8MUjAHM/B1vfwUYhoBbbyJ+e7N2yZpc8h6coCOg0bKJiN8OT4mUDkcGMsbv/g+71wRF9uqvqmIEVjhs/zOSlTe5omLSesf6Y4KZlCVy3BU3h9aN+H/12WvRdM7pLJqd469jhNGSklsHtMQXgr5+VHnNBpTlkNFpA8ysSuZPTnlXtG/dtfi4eW9MYQLMhENIpHiN930KvPYTuK7aKg6r1P7M4v4kUS2n/4siKddJRlTbC0Tbf+u2Co0la8pYk5BrBWLzYTJCHJTHup6kAlfDMOy24EeReH2mW/RHHRW4a3oQGSDOK2R test1">>
        , <<"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDAnZkz2Hcbgx0fz5eZ8DK4cXNy5zpIKqmSTlvp93KSlZ7lc+x2ohZH0uy7B/jq59IBPehCOOCWf93m6guJIzIkf9ZMGmp4AcUckHCpCUdGs9zIAGivVx13/2eVnE2GQtzsbl7l1Y85gR3TOmz6oeycKDSnnXF9qHWRUoGwqxFRhQQvU0eXKpxMgnAVNIWGFfIIwUzVUxi/a3KK01DNU6kzYfnQaqfUhl00frgh8j1Bb32Ap14wwZ5rja6UBvWsMut3lFZq7hMY0+82PWiqB4KGmHOAgmgsFMlAXD9K3DeFkz2C2MJOsYjQiHm6gIk92s3Z3F9NzOOb6Jq89x6YK3ff test2">>
        ]).

ssh_key_permission_type() ->
  oneof(['REPO_READ', 'REPO_WRITE']).

access_key() ->
  ?LET( {Permission, Text}
      , {ssh_key_permission_type(), ssh_key()}
      , #{ permission => Permission
         , text       => Text
         }).

access_keys() ->
  unique_list(access_key(), {map, text}).

%%==============================================================================
%% Webhooks
%%==============================================================================

webhook_active() ->
  boolean().

webhook_secret() ->
  non_empty(string_b()).

webhook_config() ->
  ?LET( Secret
      , webhook_secret()
      , #{ secret => Secret }).

webhook_event() ->
  oneof([<<"pr:modified">>, <<"repo:refs_changed">>]).

webhook_events() ->
  non_empty(unique_list(webhook_event())).

webhook_name() ->
  oneof([<<"webhook_a">>, <<"webhook_b">>, <<"webhook_c">>]).

webhook_url() ->
  uri().

webhook() ->
  ?LET( {Active, Config, Events, Name, Url}
      , { webhook_active()
        , webhook_config()
        , webhook_events()
        , webhook_name()
        , webhook_url()
        }
      , #{ active        => Active
         , configuration => Config
         , events        => Events
         , name          => Name
         , url           => Url
         }).

webhooks() ->
  unique_list(webhook(), {map, 'name'}).

%%==============================================================================
%% Helpers
%%==============================================================================
non_zero_nat() ->
  ?SUCHTHAT(N, nat(), N > 0).

string_b() ->
  ?LET(S, string_l(), list_to_binary(S)).

string_l() ->
  list(letter()).

letter() ->
  oneof([lowercase_letter(), uppercase_letter()]).

lowercase_letter() ->
  choose($a, $z).

sublist(L0) ->
  ?LET(L, [{E, bool()} || E <- L0], [X || {X, true} <- L]).

uppercase_letter() ->
  choose($A, $Z).

uri() ->
  ?LET(S, non_empty(string_l()), <<"https://", (list_to_binary(S))/binary>>).

unique_list(G) ->
  unique_list(G, list).

unique_list(G, {map, Key}) ->
  F = fun(ML, MR) -> maps:get(Key, ML) =< maps:get(Key, MR) end,
  ?LET(L, list(G), [X || X <- lists:usort(F, L)]);
unique_list(G, {proplist, Index}) ->
  ?LET(L, list(G), [X || X <- lists:ukeysort(Index, L)]);
unique_list(G, list) ->
  ?LET(L, list(G), [X || X <- lists:usort(L)]).

%%==============================================================================
%% Configuration for YML files
%%==============================================================================
config() ->
  ?LET( Map
      , ?LET( Keys
            , config_keys()
            , [{K, config_value(K)} || K <- Keys]
         )
      , maps:from_list(Map)
      ).

config_keys_mandatory() ->
  ['project', 'repo'].

config_keys_optional() ->
  sublist([ 'default-branch'
          , 'public'
          , 'users'
          , 'groups'
          , 'branch-restrictions'
          , 'access-keys'
          , 'hooks'
          , 'pr-restrictions'
          , 'wz-workflow'
          , 'wz-pr-restrictions'
          , 'wz-branch-reviewers'
          , 'webhooks'
          ]).

config_keys() ->
  ?LET( Optional
      , config_keys_optional()
      , config_keys_mandatory() ++ Optional
      ).

config_value('project')             -> project();
config_value('repo')                -> repo();
config_value('default-branch')      -> branch_id();
config_value('public')              -> bool();
config_value('users')               -> ss_permission_users();
config_value('groups')              -> ss_permission_groups();
config_value('branch-restrictions') -> ss_branch_restrictions();
config_value('access-keys')         -> access_keys();
config_value('hooks')               -> hooks();
config_value('pr-restrictions')     -> pr_restrictions();
config_value('wz-workflow')         -> wz_workflow();
config_value('wz-pr-restrictions')  -> wz_pr_restrictions();
config_value('wz-branch-reviewers') -> wz_branch_reviewers();
config_value('webhooks')            -> webhooks().

project() ->
  list_to_binary(os:getenv("BB_STAGING_PROJECT_KEY", "")).

repo() ->
  list_to_binary(os:getenv("BB_STAGING_REPO_SLUG", "")).

team_a() ->
  list_to_binary(os:getenv("BB_STAGING_TEAM_A", "")).

team_b() ->
  list_to_binary(os:getenv("BB_STAGING_TEAM_B", "")).

user_a() ->
  list_to_binary(os:getenv("BB_STAGING_USER_A", "")).

user_b() ->
  list_to_binary(os:getenv("BB_STAGING_USER_B", "")).
