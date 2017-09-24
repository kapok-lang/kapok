%% bitstring
-module(kapok_trans_bitstring).
-export([translate/3]).
-include("kapok.hrl").

%% Expand

%% Translate

translate(Meta, {Category, Meta1, Arg}, Ctx) when ?is_string(Category) ->
  {{bin,
    ?line(Meta),
    [{bin_element, ?line(Meta1), {string, ?line(Meta1), binary_to_list(Arg)}, default, default}]},
   Ctx};

translate(Meta, Args, Ctx) when is_list(Args) ->
  build_bitstring(fun kapok_trans:translate/2, Args, Meta, Ctx).

build_bitstring(Fun, Args, Meta, Ctx) ->
  {Result, TCtx} = build_bitstring_element(Fun, Args, Meta, Ctx, []),
  {{bin, ?line(Meta), lists:reverse(Result)}, TCtx}.

build_bitstring_element(_Fun, [], _Meta, Ctx, Acc) ->
  {Acc, Ctx};

build_bitstring_element(Fun, [Arg | Left], Meta, Ctx, Acc) ->
  {V, Size, Types} = extract_element_spec(Arg, Ctx#{context := nil}),
  build_bitstring_element(Fun, Left, Meta, Ctx, Acc, V, Size, Types).

build_bitstring_element(Fun, Args, Meta, Ctx, Acc, V, default, Types) when is_binary(V) ->
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
                ?m(Ctx, file),
                "invalid types for literal string in bitstring. Accepted types are: "
                "little, big, utf8, utf16, utf32, bits, bytes, binary, bitstring")
        end
    end,
  build_bitstring_element(Fun, Args, Meta, Ctx, [Element|Acc]);

build_bitstring_element(_Fun, _Args, Meta, Ctx, _Acc, V, _Size, _Types) when is_binary(V) ->
  kapok_error:compile_error(Meta, ?m(Ctx, file),
                            "size is not supported for literal string in bitstring");

build_bitstring_element(_Fun, _Args, Meta, Ctx, _Acc, V, _Size, _Types)
    when is_list(V); is_atom(V) ->
  kapok_error:compile_error(Meta, ?m(Ctx, file), "invalid literal ~ts in bitstring", [V]);

build_bitstring_element(Fun, Args, Meta, Ctx, Acc, V, Size, Types) ->
  {Expr, TCtx} = Fun(V, Ctx),
  case Expr of
    {bin, _, Elements} ->
      case (Size == default) andalso types_allow_splice(Types, Elements) of
        true -> build_bitstring_element(Fun, Args, Meta, TCtx, lists:reverse(Elements, Acc));
        false -> build_bitstring_element(Fun, Args, Meta, TCtx,
                                         [{bin_element, ?line(Meta), Expr, Size, Types}|Acc])
      end;
    _ ->
      build_bitstring_element(Fun, Args, Meta, TCtx,
                              [{bin_element, ?line(Meta), Expr, Size, Types}|Acc])
  end.

%% Extra bitstring element specifiers
extract_element_spec({list, _Meta, [H|T]}, Ctx) ->
  {Size, TypeSpecList} = extract_element_size_tsl(T, Ctx),
  {H, Size, TypeSpecList}.

%% Extra bitstring element size and type spec list
extract_element_size_tsl([], _Ctx) ->
  {default, default};
extract_element_size_tsl(L, _Ctx) ->
  extract_element_size_tsl(L, _Ctx, {default, []}).

extract_element_size_tsl([], _Ctx, {Size, TypeSpecList}) ->
  L = case TypeSpecList of
        [] -> default;
        X -> X
      end,
  {Size, L};
extract_element_size_tsl([{list, _Meta, [{keyword, _, size}, SizeExpr]}|T],
                         Ctx,
                         {_, TypeSpecList}) ->
  {Size, _} = kapok_trans:translate(SizeExpr, Ctx),
  extract_element_size_tsl(T, Ctx, {Size, TypeSpecList});
extract_element_size_tsl([{list, _Meta, [{keyword, _, unit}, UnitExpr]}|T],
                         Ctx,
                         {Size, TypeSpecList}) ->
  {Unit, _} = kapok_trans:translate(UnitExpr, Ctx),
  {integer, _, Value} = Unit,
  extract_element_size_tsl(T, Ctx, {Size, [{unit, Value}|TypeSpecList]});
extract_element_size_tsl([{keyword, _, Other}|T], Ctx, {Size, TypeSpecList}) ->
  extract_element_size_tsl(T, Ctx, {Size, [Other|TypeSpecList]}).

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
