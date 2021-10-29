%%==============================================================================
%% Type definition for the Workzone Pull Request Restriction data structure
%%==============================================================================
-module(bec_wz_pr_restriction_t).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ from_map/1, to_map/1]).

-include("bitbucket.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type restriction() :: #{ 'branch-id'           := bec_branch_t:id()
                        , 'approval-quota'      := integer()
                        , 'group-quota'         := integer()
                        , 'ignore-self-approve' := boolean()
                        , 'merge-condition'     := binary()
                        }.

%%==============================================================================
%% Export Types
%%==============================================================================
-export_type([ restriction/0
             ]).

%%==============================================================================
%% API
%%==============================================================================
-spec from_map(map()) -> restriction().
from_map(#{ <<"refName">>                             := RefName
          , <<"approvalQuota">>                       := ApprovalQuota
          , <<"groupQuota">>                          := GroupQuota
          , <<"ignoreContributingReviewersApproval">> := IgnoreSelfApprove
          , <<"mergeCondition">>                      := MergeCondition
          }) ->
  #{ 'branch-id'           => bec_wz_utils:strip_prefix(RefName)
   , 'approval-quota'      => binary_to_integer(ApprovalQuota)
   , 'group-quota'         => GroupQuota
   , 'ignore-self-approve' => IgnoreSelfApprove
   , 'merge-condition'     => MergeCondition
   }.

-spec to_map(restriction()) -> map().
to_map(#{ 'branch-id'      := BranchId
        } = Map) ->
  ApprovalQuota = maps:get('approval-quota', Map, 0),
  GroupQuota = maps:get('group-quota', Map, 0),
  MergeCondition = maps:get('merge-condition', Map, ""),
  IgnoreSelfApprove = maps:get('ignore-self-approve', Map, false),
  #{ <<"approvalQuota">>                       => ApprovalQuota
   , <<"approvalQuotaEnabled">>                => true
   , <<"automergeUsers">>                      => []
   , <<"deleteSourceBranch">>                  => false
   , <<"groupQuota">>                          => GroupQuota
   , <<"mergeCondition">>                      => MergeCondition
   , <<"refName">> => bec_wz_utils:add_prefix(BranchId)
   , <<"requiredBuildsCount">>                 => <<>>
   , <<"requiredSignaturesCount">>             => <<>>
   , <<"srcRefName">>                          => <<>>
   , <<"watchBuildResult">>                    => false
   , <<"ignoreContributingReviewersApproval">> => IgnoreSelfApprove
   }.
