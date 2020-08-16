%%==============================================================================
%% Type definition for the Merge Config structure
%%==============================================================================
-module(bec_merge_config_t).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ from_map/1, to_map/1]).

-include("bitbucket.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type config() :: #{ 'default-strategy'   => bec_merge_strategy_t:strategy()
                   , 'enabled-strategies' => [bec_merge_strategy_t:strategy()]
                   }.

%%==============================================================================
%% Export Types
%%==============================================================================
-export_type([ config/0
             ]).

%%==============================================================================
%% API
%%==============================================================================
-spec from_map(map()) -> config().
from_map(#{ <<"defaultStrategy">> := Default
          , <<"strategies">>      := Strategies0
          }) ->
  Strategies  = [bec_merge_strategy_t:from_map(S) || S <- Strategies0],
  #{ 'default-strategy'   => bec_merge_strategy_t:from_map(Default)
   , 'enabled-strategies' => lists:sort([S ||
                                          #{enabled := true} = S <- Strategies])
   }.

-spec to_map(config()) -> map().
to_map(#{ 'default-strategy'   := Default
        , 'enabled-strategies' := Strategies0
        }) ->
  Strategies = [bec_merge_strategy_t:to_map(S) ||
                 S <- Strategies0],
  #{ <<"defaultStrategy">> => bec_merge_strategy_t:to_map(Default)
   , <<"strategies">>      => Strategies
   }.
