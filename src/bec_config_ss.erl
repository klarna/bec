%%==============================================================================
%% Syntactic sugar for the configuration
%%==============================================================================
-module(bec_config_ss).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ to_access_keys/1
        , from_branch_restrictions/1
        , to_branch_restrictions/1
        , to_hooks/1
        , from_permission_groups/1
        , to_permission_groups/1
        , from_permission_type/1
        , from_permission_users/1
        , to_permission_users/1
        , to_pr_restrictions/1
        , to_wz_workflow/1
        , to_wz_pr_restrictions/1
        , to_wz_branch_reviewers/1
        ]).

%%==============================================================================
%% API
%%==============================================================================
-spec to_access_keys([map()]) -> [bec_ssh_key_t:key()].
to_access_keys(L) ->
  lists:sort(lists:map(fun to_access_key/1, L)).

-spec from_branch_restrictions([bec_branch_restriction_t:restriction()]) ->
        [map()].
from_branch_restrictions(L) ->
  lists:sort(lists:map(fun from_branch_restriction/1, L)).

-spec to_branch_restrictions([map()]) ->
        [bec_branch_restriction_t:restriction()].
to_branch_restrictions(L) ->
  lists:sort(lists:map(fun to_branch_restriction/1, L)).

-spec to_hooks([map()]) -> [bec_hook_t:hook()].
to_hooks(L) ->
  lists:sort(lists:map(fun atomify_keys/1, L)).

-spec from_permission_groups([bec_permission_group_t:permission()]) -> map().
from_permission_groups(Groups) ->
  lists:foldl(fun from_permission_group/2, #{}, Groups).

-spec to_permission_groups(map()) -> [bec_permission_group_t:permission()].
to_permission_groups(Map) ->
  W = [#{ permission => 'REPO_WRITE', groupname => U} ||
        U <- maps:get(<<"write">>, Map, [])],
  A = [#{ permission => 'REPO_ADMIN', groupname => U} ||
        U <- maps:get(<<"admin">>, Map, [])],
  R = [#{ permission => 'REPO_READ' , groupname => U} ||
        U <- maps:get(<<"read">>, Map, [])],
  lists:sort(W ++ A ++ R).

-spec from_permission_users([bec_permission_user_t:permission()]) -> map().
from_permission_users(Users) ->
  lists:foldl(fun from_permission_user/2, #{}, Users).

-spec to_permission_users(map()) -> [bec_permission_user_t:permission()].
to_permission_users(Map) ->
  W = [#{ permission => 'REPO_WRITE', username => U} ||
        U <- maps:get(<<"write">>, Map, [])],
  A = [#{ permission => 'REPO_ADMIN', username => U} ||
        U <- maps:get(<<"admin">>, Map, [])],
  R = [#{ permission => 'REPO_READ' , username => U} ||
        U <- maps:get(<<"read">>, Map, [])],
  lists:sort(W ++ A ++ R).

-spec to_pr_restrictions(map()) -> bec_pr_restriction_t:restriction().
to_pr_restrictions(#{ <<"merge-config">> := MergeConfig
                    } = R0) ->
  R = R0#{<<"merge-config">> => to_merge_config(MergeConfig)},
  atomify_keys(R).

-spec to_merge_config(map()) -> bec_merge_config_t:config().
to_merge_config(#{ <<"default-strategy">>   := Default
                 , <<"enabled-strategies">> := Strategies
                 }) ->
  #{ 'default-strategy'   => to_merge_strategy(Default)
   , 'enabled-strategies' => to_merge_strategies(Strategies)
   }.

-spec to_merge_strategies([map()]) -> [bec_merge_strategy_t:strategy()].
to_merge_strategies(L) ->
  lists:sort(lists:map(fun to_merge_strategy/1, L)).

-spec to_merge_strategy(binary()) -> bec_merge_strategy_t:strategy().
to_merge_strategy(Id) ->
  #{ enabled => true
   , id      => binary_to_atom(Id, utf8)
   }.

-spec to_wz_workflow(map()) -> bec_wz_workflow_t:workflow().
to_wz_workflow(#{ <<"push-after-pr">> := PAP
                , <<"unapprove-pr">>  := UPR
                }) ->
  #{ 'push-after-pr' => PAP
   , 'unapprove-pr'  => UPR
   }.

-spec to_wz_pr_restrictions([map()]) -> [bec_wz_pr_restriction_t:restriction()].
to_wz_pr_restrictions(L) ->
  lists:sort(lists:map(fun atomify_keys/1, L)).

-spec to_wz_branch_reviewers([map()]) -> [bec_wz_branch_reviewer_t:reviewer()].
to_wz_branch_reviewers(L) ->
  lists:sort(lists:map(fun to_wz_branch_reviewer/1, L)).

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec to_access_key(map()) -> bec_ssh_key_t:key().
to_access_key(#{ <<"permission">> := Permission
               , <<"text">>       := Text
               }) ->
  #{ permission => binary_to_atom(Permission, utf8)
   , text       => list_to_binary(
                     string:strip(
                       binary_to_list(Text), right, $\n))
   }.

-spec from_branch_restriction(bec_branch_restriction_t:restriction()) -> map().
from_branch_restriction(#{ users  := Users
                         , groups := Groups
                         } = R0) ->
  R = R0#{ exceptions => #{ users => Users
                          , groups => Groups
                          }
         },
  maps:remove(users, maps:remove(groups, R)).

-spec to_branch_restriction(map()) -> bec_branch_restriction_t:restriction().
to_branch_restriction(#{ <<"matcher-type">> := MT
                       , <<"type">>         := T
                       } = R0) ->
  Exceptions = maps:get(<<"exceptions">>, R0, #{ <<"users">>  => []
                                               , <<"groups">> => []}),
  Users  = maps:get(<<"users">>,  Exceptions, []),
  Groups = maps:get(<<"groups">>, Exceptions, []),
  R = maps:remove( <<"exceptions">>
                 , R0#{ <<"matcher-type">> => binary_to_atom(MT, utf8)
                      , <<"type">>         => binary_to_atom(T,  utf8)
                      , <<"users">>        => lists:usort(Users)
                      , <<"groups">>       => lists:usort(Groups)
                      }),
  atomify_keys(R).

-spec from_permission_group(bec_permission_group_t:permission(), map()) ->
        map().
from_permission_group(#{ permission := Permission0
                       , groupname  := Groupname
                       }, Acc) ->
  Permission = from_permission_type(Permission0),
  Groupnames  = maps:get(Permission, Acc, []),
  maps:put(Permission, [Groupname|Groupnames], Acc).

-spec from_permission_user(bec_permission_user_t:permission(), map()) ->
        map().
from_permission_user(#{ permission := Permission0
                      , username   := Username
                      }, Acc) ->
  Permission = from_permission_type(Permission0),
  Usernames  = maps:get(Permission, Acc, []),
  maps:put(Permission, [Username|Usernames], Acc).

-spec from_permission_type(bec_permission_user_t:permission_type()) -> atom().
from_permission_type('REPO_ADMIN') -> admin;
from_permission_type('REPO_WRITE') -> write;
from_permission_type('REPO_READ')  -> read.

-spec to_wz_branch_reviewer(map()) -> bec_wz_branch_reviewer_t:reviewer().
to_wz_branch_reviewer(#{<<"paths">> := Paths} = R0) ->
  R = R0#{<<"paths">> => lists:sort([atomify_keys(P) || P <- Paths])},
  Mandatory = #{<<"mandatory-users">>  => []
              , <<"mandatory-groups">> => []
              },
  Merged = maps:merge(Mandatory, R),
  atomify_keys(Merged).

-spec atomify_keys(map()) -> map().
atomify_keys(Map) ->
  F = fun(K, V, Acc) ->
          maps:put(binary_to_atom(K, utf8), V, Acc)
      end,
  maps:fold(F, #{}, Map).
