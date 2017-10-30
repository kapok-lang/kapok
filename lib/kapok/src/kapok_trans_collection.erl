%% For translating collections.
-module(kapok_trans_collection).
-export([build_tuple/2,
         build_map_from/2,
         build_list/1,
         translate_tuple/3,
         translate_map/3,
         translate_set/3,
         translate_list/2,
         translate_cons_list/3
        ]).
-import(kapok_scanner, [token_meta/1]).
-include("kapok.hrl").

%% tuple
translate_tuple(Meta, Arg, Ctx) ->
  {TArg, TCtx} = kapok_trans:translate(Arg, Ctx),
  {build_tuple(Meta, TArg), TCtx}.

build_tuple(Meta, TArg) when is_list(Meta) ->
  build_tuple(?line(Meta), TArg);
build_tuple(Line, TArg) when is_integer(Line) ->
  {tuple, Line, TArg}.

%% map
translate_map(Meta, Args, #{context := Context} = Ctx) ->
  FieldType = case Context of
                C when C == fn_pattern; C == let_pattern; C == case_pattern ->
                  map_field_exact;
                _ ->
                  map_field_assoc
              end,
  {TFields, TCtx} = build_map(Meta, FieldType, Args, Ctx),
  {{map, ?line(Meta), TFields}, TCtx}.

build_map(Meta, FieldType, Args, Ctx) ->
  build_map(Meta, FieldType, Args, [], Ctx).

build_map(_Meta, _FieldType, [], Acc, Ctx) ->
  {lists:reverse(Acc), Ctx};
build_map(Meta, _FieldType, [H], _Acc, Ctx) ->
  kapok_error:compile_error(Meta, ?m(Ctx, file), "unpaired values in map ~p", [H]);
build_map(Meta, FieldType, [K, V | Left], Acc, Ctx) ->
  {TK, TCtx} = kapok_trans:translate(K, Ctx),
  {TV, TCtx1} = kapok_trans:translate(V, TCtx),
  Field = {FieldType, ?line(kapok_scanner:token_meta(K)), TK, TV},
  build_map(Meta, FieldType, Left, [Field | Acc], TCtx1).

build_map_from(Meta, TranslatedPairs) ->
  TFields = lists:map(fun({{_, Line, _} = K, V}) ->
                          {map_field_assoc, Line, K, V}
                      end,
                      TranslatedPairs),
  {map, ?line(Meta), TFields}.

%% set
translate_set(Meta, Args, #{context := Context} = Ctx) ->
  case Context of
    C when C == fn_pattern; C == let_pattern; C == case_pattern ->
      kapok_error:compile_error(Meta, ?m(Ctx, file), "unsupported set in pattern");
    _ -> ok
  end,
  {TArgs, TCtx} = translate_list(Args, Ctx),
  kapok_trans:translate_remote_call(Meta, 'gb_sets', 'from_list', [TArgs], TCtx).

%% list
translate_list({Category, Meta, List}, Ctx) when ?is_list(Category) ->
  {TList, TCtx} = translate_list(List, Ctx),
  {{Category, Meta, TList}, TCtx};
translate_list(L, Ctx) when is_list(L) ->
  translate_list(L, [], Ctx).
translate_list([H|T], Acc, Ctx) ->
  {Erl, TCtx} = kapok_trans:translate(H, Ctx),
  translate_list(T, [Erl|Acc], TCtx);
translate_list([], Acc, Ctx) ->
  {build_list_reversed(Acc), Ctx}.

build_list(L) ->
  build_list_reversed(lists:reverse(L)).
build_list_reversed(R) ->
  build_list_reversed(R, {nil, 0}).
build_list_reversed([H|T], Acc) ->
  build_list_reversed(T, {cons, 0, H, Acc});
build_list_reversed([], Acc) ->
  Acc.

%% cons_list
translate_cons_list(Head, Tail, Ctx) ->
  translate_cons_list(Head, [], Tail, Ctx).

translate_cons_list([], Acc, Tail, Ctx) ->
  {TTail, TCtx} = kapok_trans:translate(Tail, Ctx),
  L = lists:foldl(fun(X, Acc1) ->
                      Line = erlang:element(2, X),
                      {cons, Line, X, Acc1}
                  end,
                  TTail,
                  Acc),
  {L, TCtx};
translate_cons_list([H | T], Acc, Tail, Ctx) ->
  {TH, TCtx} = kapok_trans:translate(H, Ctx),
  translate_cons_list(T, [TH | Acc], Tail, TCtx).
