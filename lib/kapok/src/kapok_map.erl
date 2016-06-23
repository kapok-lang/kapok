%% map
-module(kapok_map).
-export([translate/3,
         build_map/2]).
-include("kapok.hrl").

%% Translate

translate(Meta, Args, Env) when is_list(Args) ->
  build_map(Meta, Args, Env).

build_map(Meta, TranslatedPairs) ->
  TFields = lists:map(fun({{_, Line, _} = K, V}) ->
                         {map_field_assoc, Line, K, V}
                     end,
                     TranslatedPairs),
  {map, ?line(Meta), TFields}.

build_map(Meta, Args, #{context := Context} = Env) ->
  FieldType = case Context of
                match_vars -> map_field_exact;
                _ -> map_field_assoc
              end,
  {TFields, TEnv} = build_map_field(Meta, FieldType, Args, Env),
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


