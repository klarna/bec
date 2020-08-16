%%==============================================================================
%% Type definition for the Branch Restriction data structure
%%==============================================================================
-module(bec_branch_restriction_t).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ from_map/1, to_map/1 ]).

-include("bitbucket.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type id()          :: integer().
-type restriction() :: #{ id             := id()
                        , 'matcher-type' => 'BRANCH' | 'PATTERN'
                        , 'branch-id'    => bec_branch_t:id()
                        , type           => 'fast-forward-only'
                                          | 'no-deletes'
                                          | 'pull-request-only'
                                          | 'read-only'
                        , users          => [bec_user_t:name()]
                        , groups         => [bec_group_t:name()]
                        }.

%%==============================================================================
%% Export Types
%%==============================================================================
-export_type([ id/0
             , restriction/0
             ]).

%%==============================================================================
%% API
%%==============================================================================
-spec from_map(map()) -> restriction().
from_map(#{ <<"id">>      := Id
          , <<"groups">>  := Groups
          , <<"matcher">> := #{ <<"id">>   := MatcherId
                              , <<"type">> := #{<<"id">> := MatcherType}}
          , <<"type">>    := Type
          , <<"users">>   := Users
          }) ->
  #{ id             => Id
   , 'matcher-type' => binary_to_atom(MatcherType, utf8)
   , 'branch-id'    => MatcherId
   , type           => binary_to_atom(Type, utf8)
   , users          => lists:sort([bec_user_t:from_map(U) || U <- Users])
   , groups         => lists:sort(Groups)
   }.

-spec to_map(restriction()) -> map().
to_map(#{ 'matcher-type' := MatcherType
        , 'branch-id'    := BranchId
        , type           := Type
        , users          := Users
        , groups         := Groups
        }) ->
  MatcherTypeName = matcher_type_name(MatcherType),
  #{ <<"type">>    => Type
   , <<"matcher">> => #{ <<"id">>        => BranchId
                       , <<"displayId">> => BranchId
                       , <<"type">>      => #{ <<"id">>   => MatcherType
                                             , <<"name">> => MatcherTypeName
                                             }
                       , <<"active">>    => true
                       }
   , <<"users">>   => Users
   , <<"groups">>  => Groups
   }.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec matcher_type_name(atom()) -> binary().
matcher_type_name('BRANCH')  -> <<"Branch">>;
matcher_type_name('PATTERN') -> <<"Pattern">>.
