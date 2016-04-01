%% binary
-module(kapok_binary).
-export([translate/3]).
-include("kapok.hrl").

%% Translation

translate(Meta, Args, Scope) ->
  case Scope#kapok_scope.context of
    match ->
      build_binary(fun kapok_translator:translate/2, Args, Meta, Scope);
    _ ->
      build_binary(fun(X, Acc) -> kapok_translator:translate_arg(X, Acc, Scope) end,
                   Args,
                   Meta,
                   Scope)
  end.

build_binary(Fun, Args, Meta, Scope) ->
  {Result, NewScope} = build_binary_each(Fun, Args, Meta, Scope, []),
  {{bin, ?line(Meta), lists:reverse(Result)}, NewScope}.

build_binary_each(_Fun, [], _Meta, Scope, Acc) ->
  {Acc, Scope};

build_binary_each(Fun, [Arg | Left], Meta, Scope, Acc) ->
  {V, Size, Types} = extract_element_spec(Arg, Scope#kapok_scope{context=nil}),
  build_binary_each(Fun, Left, Meta, Scope, Acc, V, Size, Types).

build_binary_each(Fun, Args, Meta, Scope, Acc, V, default, Types) when is_binary(V) ->
  Element =
    case types_allow_splice(Types, []) of
      true ->
        {bin_element, ?line(Meta), {string, 0, binary_to_list(V)}};
      false ->
        case types_require_conversion(Types) of
          true ->
            {bin_element, ?line(Meta), {string, 0, kapok_utils:characters_to_list(V)}, default, Types};
          false ->
            kapok_error:compile_error(
                Meta, Scope#kapok_scope.file, "invalid types for literal string in binary. "
                "Accepted types are: little, big, utf8, utf16, utf32, bits, bytes, binary, bitstring")
        end
    end,
  build_binary_each(Fun, Args, Meta, Scope, [Element|Acc]);

build_binary_each(_Fun, _Args, Meta, Scope, _Acc, V, _Size, _Types) when is_binary(V) ->
  kapok_error:compile_error(Meta, Scope#kapok_scope.file, "size is not supported for literal string in binary");

build_binary_each(_Fun, _Args, Meta, Scope, _Acc, V, _Size, _Types) when is_list(V); is_atom(V) ->
  kapok_error:compile_error(Meta, Scope#kapok_scope.file, "invalid literal ~ts in binary", [V]);

build_binary_each(Fun, Args, Meta, Scope, Acc, V, Size, Types) ->
  {Expr, NewScope} = Fun(V, Scope),
  case Expr of
    {bin, _, Elements} ->
      case (Size == default) andalso types_allow_splice(Types, Elements) of
        true -> build_binary_each(Fun, Args, Meta, NewScope, lists:reverse(Elements, Acc));
        false -> build_binary_each(Fun, Args, Meta, NewScope, [{bin_element, ?line(Meta), Expr, Size, Types}|Acc])
      end;
    _ ->
      build_binary_each(Fun, Args, Meta, NewScope, [{bin_element, ?line(Meta), Expr, Size, Types}|Acc])
  end.

%% Extra binary element specifiers
extract_element_spec({list, _Meta, [H|T]}, Scope) ->
  {Size, TypeSpecList} = extract_element_size_tsl(T, Scope),
  {H, Size, TypeSpecList}.

%% Extra binary element size and type spec list
extract_element_size_tsl([], _Scope) ->
  {default, default};
extract_element_size_tsl(L, _Scope) ->
  extract_element_size_tsl(L, _Scope, {default, []}).

extract_element_size_tsl([], _Scope, {Size, TypeSpecList}) ->
  L = case TypeSpecList of
        [] -> default;
        X -> X
      end,
  {Size, L};
extract_element_size_tsl([{list, _Meta, [{atom, _, size}, SizeExpr]}|T], Scope, {_, TypeSpecList}) ->
  {Size, _} = kapok_translator:translate(SizeExpr, Scope),
  extract_element_size_tsl(T, Scope, {Size, TypeSpecList});
extract_element_size_tsl([{list, _Meta, [{atom, _, unit}, UnitExpr]}|T], Scope, {Size, TypeSpecList}) ->
  {Unit, _} = kapok_translator:translate(UnitExpr, Scope),
  {integer, _, Value} = Unit,
  extract_element_size_tsl(T, Scope, {Size, [{unit, Value}|TypeSpecList]});
extract_element_size_tsl([{atom, _, Other}|T], Scope, {Size, TypeSpecList}) ->
  extract_element_size_tsl(T, Scope, {Size, [Other|TypeSpecList]}).

%% Check whether the given type rerquire conversion
types_require_conversion([End|T]) when End == little; End == big ->
  types_require_conversion(T);
types_require_conversion([UTF|T]) when UTF == utf8; UTF == utf16; UTF == utf32 ->
  types_require_conversion(T);
types_require_conversion([]) -> true;
types_require_conversion(_) -> false.

%% Check whether the given type allows splice.
types_allow_splice([bytes], Elements)  -> is_byte_size(Elements, 0);
types_allow_splice([binary], Elements) -> is_byte_size(Elements, 0);
types_allow_splice([bits], _)          -> true;
types_allow_splice([bitstring], _)     -> true;
types_allow_splice(default, _)         -> true;
types_allow_splice(_, _)               -> false.

%% check the total size of all elements in a binary.
is_byte_size([Element|T], Acc) ->
  case element_size(Element) of
    {unknown, Unit} when Unit rem 8 == 0 -> is_byte_size(T, Acc);
    {unknown, _Unit} -> false;
    {Size, Unit} -> is_byte_size(T, Size * Unit + Acc)
  end;
is_byte_size([], Size) ->
  Size rem 8 == 0.

%% get the size of element
element_size({bin_element, _, _, default, Types})  -> {unknown, unit_size(Types)};
element_size({bin_element, _, _, {integer, _, Size}, Types}) -> {Size, unit_size(Types)};
element_size({bin_element, _, _, _Size, Types}) -> {unknown, unit_size(Types)}.

%% get unit size
%% The default value of `Type' is integer,
%% and the default value of `Unit' for integer type is 1.
unit_size(Types)               -> unit_size(Types, 1).
unit_size([binary|T], _)       -> unit_size(T, 8);
unit_size([{unit, Size}|_], _) -> Size;
unit_size([_|T], Guess)        -> unit_size(T, Guess);
unit_size([], Guess)           -> Guess.

