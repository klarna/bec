%%==============================================================================
%% Encode Erlang terms into YML files.
%%==============================================================================
%% Extracted and modified from:
%% https://github.com/processone/fast_yaml/blob/master/src/fast_yaml.erl
%%==============================================================================
-module(bec_yml).

-export([ decode/1
        , decode_file/1
        , encode/1
        , to_mustache/1
        ]).

decode(String) ->
  yamerl:decode(String, options()).

decode_file(Path) ->
  yamerl:decode_file(Path, options()).

options() ->
  [ str_node_as_binary
  , {map_node_format, map}
  ].

encode(Term) ->
  NL = nl(),
  case encode(Term, 0) of
    [[NL|T1]|T2] -> [T1|T2];
    T -> T
  end.

to_mustache(Binary) when is_binary(Binary) -> binary_to_list(Binary);
to_mustache(Atom) when is_atom(Atom)       -> Atom;
to_mustache(Number) when is_number(Number) -> Number.

encode([{_, _}|_] = Terms, N) ->
  [[nl(), indent(N), encode_pair(T, N)] || T <- Terms];
encode([_|_] = Terms, N) ->
  [[nl(), indent(N), "- ", encode(T, N+2)] || T <- Terms];
encode([], _) ->
  "[]";
encode(I, _) when is_integer(I) ->
  integer_to_list(I);
encode(F, _) when is_float(F) ->
  io_lib:format("~f", [F]);
encode(A, _) when is_atom(A) ->
  atom_to_list(A);
encode(B, _) when is_binary(B) ->
  [$",
   lists:map(
     fun($") -> [$\\, $"];
        (C) -> C
     end, binary_to_list(B)),
   $"];
encode(M, N) when is_map(M) ->
  L = maps:to_list(M),
  [nl(), indent(N), [[encode_pair({K, V}, N), nl(), indent(N)] || {K, V} <- L]].

encode_pair({K, V}, N) ->
  [encode(K), ": ", encode(V, N+2)].

nl() ->
  io_lib:nl().

indent(N) ->
  lists:duplicate(N, $ ).
