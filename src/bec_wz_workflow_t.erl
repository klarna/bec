%%==============================================================================
%% Type definition for the Workzone Workflow data structure
%%==============================================================================
-module(bec_wz_workflow_t).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ from_map/1, to_map/1]).

-include("bitbucket.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type workflow() :: #{ 'push-after-pr' := boolean()
                     , 'unapprove-pr'  := boolean()
                     }.

%%==============================================================================
%% Export Types
%%==============================================================================
-export_type([ workflow/0
             ]).

%%==============================================================================
%% API
%%==============================================================================
-spec from_map(map()) -> workflow().
from_map(#{ <<"pushAfterPullReq">> := PushAfterPR
          , <<"unapprovePullReq">> := UnapprovePR
          }) ->
  #{ 'push-after-pr' => PushAfterPR
   , 'unapprove-pr'  => UnapprovePR
   }.

-spec to_map(workflow()) -> map().
to_map(#{ 'push-after-pr' := PushAfterPR
        , 'unapprove-pr'  := UnapprovePR
        }) ->
  #{ <<"pushAfterPullReq">> => PushAfterPR
   , <<"unapprovePullReq">> => UnapprovePR
   }.
