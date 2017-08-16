%% bitstring
-module(kapok_trans_bitstring).
-export([translate/3]).
-include("kapok.hrl").

%% Expand

%% Translate

translate(Meta, {Category, Meta1, Arg}, Env) when ?is_string(Category) ->
  {{bin,
    ?line(Meta),
    [{bin_element, ?line(Meta1), {string, ?line(Meta1), binary_to_list(Arg)}, default, default}]},
   Env};

translate(Meta, Args, Env) when is_list(Args) ->
  build_bitstring(fun kapok_trans:translate/2, Args, Meta, Env).

build_bitstring(Fun, Args, Meta, Env) ->
  {Result, TEnv} = build_bitstring_element(Fun, Args, Meta, Env, []),
  {{bin, ?line(Meta), lists:reverse(Result)}, TEnv}.

build_bitstring_element(_Fun, [], _Meta, Env, Acc) ->
  {Acc, Env};

build_bitstring_element(Fun, [Arg | Left], Meta, Env, Acc) ->
  {V, Size, Types} = extract_element_spec(Arg, Env#{context := nil}),
  build_bitstring_element(Fun, Left, Meta, Env, Acc, V, Size, Types).

build_bitstring_element(Fun, Args, Meta, Env, Acc, V, default, Types) when is_binary(V) ->
  Element =
    case types_allow_splice(Types, []) of
      true ->
        {bin_element, ?line(Meta), {string, 0, binary_to_list(V)}};
      false ->
        case types_require_conversion(Types) of
          true ->
            {bin_element,
             ?line(Meta),
             {string, 0, kapok_utils:characters_to_list(V)},
             default,
             Types};
          false ->
            kapok_error:compile_error(
                Meta,
                ?m(Env, file),
                "invalid types for literal string in bitstring. Accepted types are: "
                "little, big, utf8, utf16, utf32, bits, bytes, binary, bitstring")
        end
    end,
  build_bitstring_element(Fun, Args, Meta, Env, [Element|Acc]);

build_bitstring_element(_Fun, _Args, Meta, Env, _Acc, V, _Size, _Types) when is_binary(V) ->
  kapok_error:compile_error(Meta, ?m(Env, file),
                            "size is not supported for literal string in bitstring");

build_bitstring_element(_Fun, _Args, Meta, Env, _Acc, V, _Size, _Types)
    when is_list(V); is_atom(V) ->
  kapok_error:compile_error(Meta, ?m(Env, file), "invalid literal ~ts in bitstring", [V]);

build_bitstring_element(Fun, Args, Meta, Env, Acc, V, Size, Types) ->
  {Expr, TEnv} = Fun(V, Env),
  case Expr of
    {bin, _, Elements} ->
      case (Size == default) andalso types_allow_splice(Types, Elements) of
        true -> build_bitstring_element(Fun, Args, Meta, TEnv, lists:reverse(Elements, Acc));
        false -> build_bitstring_element(Fun, Args, Meta, TEnv,
                                         [{bin_element, ?line(Meta), Expr, Size, Types}|Acc])
      end;
    _ ->
      build_bitstring_element(Fun, Args, Meta, TEnv,
                              [{bin_element, ?line(Meta), Expr, Size, Types}|Acc])
  end.

%% Extra bitstring element specifiers
extract_element_spec({list, _Meta, [H|T]}, Env) ->
  {Size, TypeSpecList} = extract_element_size_tsl(T, Env),
  {H, Size, TypeSpecList}.

%% Extra bitstring element size and type spec list
extract_element_size_tsl([], _Env) ->
  {default, default};
extract_element_size_tsl(L, _Env) ->
  extract_element_size_tsl(L, _Env, {default, []}).

extract_element_size_tsl([], _Env, {Size, TypeSpecList}) ->
  L = case TypeSpecList of
        [] -> default;
        X -> X
      end,
  {Size, L};
extract_element_size_tsl([{list, _Meta, [{keyword, _, size}, SizeExpr]}|T],
                         Env,
                         {_, TypeSpecList}) ->
  {Size, _} = kapok_translator:translate(SizeExpr, Env),
  extract_element_size_tsl(T, Env, {Size, TypeSpecList});
extract_element_size_tsl([{list, _Meta, [{keyword, _, unit}, UnitExpr]}|T],
                         Env,
                         {Size, TypeSpecList}) ->
  {Unit, _} = kapok_translator:translate(UnitExpr, Env),
  {integer, _, Value} = Unit,
  extract_element_size_tsl(T, Env, {Size, [{unit, Value}|TypeSpecList]});
extract_element_size_tsl([{keyword, _, Other}|T], Env, {Size, TypeSpecList}) ->
  extract_element_size_tsl(T, Env, {Size, [Other|TypeSpecList]}).

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

%% check the total size of all elements in a bitstring.
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
