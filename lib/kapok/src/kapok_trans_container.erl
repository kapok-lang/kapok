%% container
-module(kapok_trans_container).
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
translate_tuple(Meta, Arg, Env) ->
  {TArg, TEnv} = kapok_trans:translate(Arg, Env),
  {build_tuple(Meta, TArg), TEnv}.

build_tuple(Meta, TArg) when is_list(Meta) ->
  build_tuple(?line(Meta), TArg);
build_tuple(Line, TArg) when is_integer(Line) ->
  {tuple, Line, TArg}.

%% map
translate_map(Meta, Args, #{context := Context} = Env) ->
  FieldType = case Context of
                pattern -> map_field_exact;
                _ -> map_field_assoc
              end,
  {TFields, TEnv} = build_map(Meta, FieldType, Args, Env),
  {{map, ?line(Meta), TFields}, TEnv}.

build_map(Meta, FieldType, Args, Env) ->
  build_map(Meta, FieldType, Args, [], Env).

build_map(_Meta, _FieldType, [], Acc, Env) ->
  {lists:reverse(Acc), Env};
build_map(Meta, _FieldType, [H], _Acc, Env) ->
  kapok_error:compile_error(Meta, ?m(Env, file), "unpaired values in map ~p", [H]);
build_map(Meta, FieldType, [K, V | Left], Acc, Env) ->
  {TK, TEnv} = kapok_trans:translate(K, Env),
  {TV, TEnv1} = kapok_trans:translate(V, TEnv),
  Field = {FieldType, ?line(kapok_scanner:token_meta(K)), TK, TV},
  build_map(Meta, FieldType, Left, [Field | Acc], TEnv1).

build_map_from(Meta, TranslatedPairs) ->
  TFields = lists:map(fun({{_, Line, _} = K, V}) ->
                          {map_field_assoc, Line, K, V}
                      end,
                      TranslatedPairs),
  {map, ?line(Meta), TFields}.

%% set
translate_set(_Meta, Arg, Env) ->
  %% TODO add impl
  {Arg, Env}.

%% list
translate_list({Category, Meta, List}, Env) when ?is_list(Category) ->
  {TList, TEnv} = translate_list(List, Env),
  {{Category, Meta, TList}, TEnv};
translate_list(L, Env) when is_list(L) ->
  translate_list(L, [], Env).
translate_list([H|T], Acc, Env) ->
  {Erl, TEnv} = kapok_trans:translate(H, Env),
  translate_list(T, [Erl|Acc], TEnv);
translate_list([], Acc, Env) ->
  {build_list_reversed(Acc), Env}.

build_list(L) ->
  build_list_reversed(lists:reverse(L)).
build_list_reversed(R) ->
  build_list_reversed(R, {nil, 0}).
build_list_reversed([H|T], Acc) ->
  build_list_reversed(T, {cons, 0, H, Acc});
build_list_reversed([], Acc) ->
  Acc.

%% cons_list
translate_cons_list(Head, Tail, Env) ->
  translate_cons_list(Head, [], Tail, Env).

translate_cons_list([], Acc, Tail, Env) ->
  {TTail, TEnv} = kapok_trans:translate(Tail, Env),
  L = lists:foldl(fun(X, Acc1) ->
                      Line = erlang:element(2, X),
                      {cons, Line, X, Acc1}
                  end,
                  TTail,
                  Acc),
  {L, TEnv};
translate_cons_list([H | T], Acc, Tail, Env) ->
  {TH, TEnv} = kapok_trans:translate(H, Env),
  translate_cons_list(T, [TH | Acc], Tail, TEnv).
