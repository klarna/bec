%%==============================================================================
%% YAML node type for accessing OS environment variables.
%%==============================================================================
-module(bec_node_env_variable).

-include_lib("yamerl/include/yamerl_errors.hrl").
-include_lib("yamerl/include/yamerl_tokens.hrl").
-include_lib("yamerl/include/yamerl_nodes.hrl").
-include_lib("yamerl/include/internal/yamerl_constr.hrl").

-export([ tags/0
        , construct_token/3
        , node_pres/1
        ]).

-define(TAG, "tag:bec:env").

tags() -> [?TAG, "!env"].

construct_token(#yamerl_constr{detailed_constr = Detailed},
               undefined,
               #yamerl_scalar{text = Text} = Token) ->
  case os:getenv(Text) of
    false ->
      parsing_error(Token, "Undefined OS environment variable");
    Value when Detailed ->
      Pres = yamerl_constr:get_pres_details(Token),
      Node = #yamerl_str{ module = ?MODULE
                        , tag = ?TAG
                        , pres = Pres
                        , text = Value
                        },
      {finished, Node};
    Value ->
      {finished, Value}
  end;
construct_token(_, _, Token) ->
  parsing_error(Token, "Invalid OS environment variable").

node_pres(Node) ->
    ?NODE_PRES(Node).

parsing_error(Token, Text) ->
  Error = #yamerl_parsing_error{
    name   = not_an_os_env,
    token  = Token,
    text   = Text,
    line   = ?TOKEN_LINE(Token),
    column = ?TOKEN_COLUMN(Token)
  },
  throw(Error).
