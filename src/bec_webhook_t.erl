%%==============================================================================
%% Type definition for the Webhook data structure
%%==============================================================================
-module(bec_webhook_t).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ from_map/1
        , to_map/1
        ]).

%%==============================================================================
%% Types
%%==============================================================================
-type id() :: pos_integer().
-type webhook() :: #{ id            => id()
                    , createdDate   => pos_integer()
                    , updatedDate   => pos_integer()
                    , active        := boolean()
                    , configuration := #{ secret := binary() }
                    , events        := [binary()]
                    , name          := binary()
                    , url           := binary()
                    }.

%%==============================================================================
%% Export Types
%%==============================================================================
-export_type([ webhook/0
             , id/0
             ]).

%%==============================================================================
%% API
%%==============================================================================
-spec from_map(map()) -> webhook().
from_map(#{ <<"configuration">> := Config
          , <<"events">>        := Events
          } = Map) ->
  Map0 = maps:remove(<<"sslVerificationRequired">>, Map),
  Map1 = maps:remove(<<"scopeType">>, Map0),
  keys_to_atoms(Map1#{ <<"configuration">> => keys_to_atoms(Config)
                     , <<"events">>        => lists:sort(Events)
                     }).

-spec to_map(webhook()) -> map().
to_map(#{ configuration := Config
        , events        := Events
        } = WebHook) ->
  keys_to_binaries(WebHook#{ configuration => keys_to_binaries(Config)
                           , events        => lists:sort(Events)
                           }).

%% These conversions are un-necessary, but getting rid of them would
%% require a bigger BEC refactoring.
-spec keys_to_atoms(#{ binary() => any() }) -> #{ atom() => any() }.
keys_to_atoms(Map) ->
  F = fun(K, M) ->
          M#{ binary_to_atom(K, utf8) => maps:get(K, Map)}
      end,
  lists:foldl(F, #{}, maps:keys(Map)).

-spec keys_to_binaries(#{ atom() => any() }) -> #{ binary() => any() }.
keys_to_binaries(Map) ->
  F = fun(K, M) ->
          M#{ list_to_binary(atom_to_list(K)) => maps:get(K, Map)}
      end,
  lists:foldl(F, #{}, maps:keys(Map)).
