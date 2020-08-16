%%==============================================================================
%% Utility functions used to interact with the Workzone plugin
%%==============================================================================
-module(bec_wz_utils).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ add_prefix/1
        , strip_prefix/1
        ]).

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec add_prefix(bec_branch_t:id()) -> bec_branch_t:id().
add_prefix(<<"refs/heads/", _/binary>> = Id) -> Id;
add_prefix(Id) when is_binary(Id)            -> <<"refs/heads/", Id/binary>>.

-spec strip_prefix(bec_branch_t:id()) -> bec_branch_t:id().
strip_prefix(<<"refs/heads/", Id/binary>>) -> Id;
strip_prefix(Id) when is_binary(Id)        -> Id.
