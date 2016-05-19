%% map
-module(kapok_map).
-export([translate/3]).
-include("kapok.hrl").

%% Translate

translate(Meta, Args, Env) when is_list(Args) ->
  build_map(Meta, Args, Env).

build_map(Meta, Args, Env) ->
  %% treat all map fields as assoc fields
  {TFields, TEnv} = build_map_field(Meta, map_field_assoc, Args, Env),
  {{map, ?line(Meta), TFields}, TEnv}.

build_map_field(Meta, FieldType, Args, Env) ->
  build_map_field(Meta, FieldType, Args, [], Env).

build_map_field(_Meta, _FieldType, [], Acc, Env) ->
  {lists:reverse(Acc), Env};
build_map_field(Meta, _FieldType, [H], _Acc, Env) ->
  kapok_error:compile_error(Meta, ?m(Env, file), "unpaired values in map ~p", [H]);
build_map_field(Meta, FieldType, [K, V | Left], Acc, Env) ->
  {TK, TEnv} = kapok_translate:translate(K, Env),
  {TV, TEnv1} = kapok_translate:translate(V, TEnv),
  Field = {FieldType, ?line(kapok_scanner:token_meta(K)), TK, TV},
  build_map_field(Meta, FieldType, Left, [Field | Acc], TEnv1).


