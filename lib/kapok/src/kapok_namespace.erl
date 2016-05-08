%%
-module(kapok_namespace).
-export([translate/3,
         init_namespace_table/0,
         add_namespace/1,
         add_export/2,
         namespace_functions/1,
         namespace_macros/1,
         namespace_exports/1,
         export_forms/1
        ]).

-include("kapok.hrl").


%% helpers

export_forms(Namespace) ->
  Exports = namespace_exports(Namespace),
  {attribute,0,export,sets:to_list(Exports)}.


%% namespace table

init_namespace_table() ->
  _ = ets:new(kapok_namespaces, [set, protected, named_table, {read_concurrency, true}]).

add_namespace(Tuple) ->
  ets:insert(kapok_namespaces, Tuple).

namespace_functions(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 2).

namespace_macros(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 3).

add_export(Namespace, Export) ->
  OldExports = namespace_exports(Namespace),
  NewExports = sets:add_element(Export, OldExports),
  ets:update_element(kapok_namespaces, Namespace, {4, NewExports}).

namespace_exports(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 4).

%% Translation

translate(Meta, [{identifier, _, "ns"}], Env) ->
  kapok_error:compile_error(Meta, ?m(Env, file), "no namespace");

translate(Meta, [{identifier, _, "ns"}, {identifier, _, Id}|T], Env) ->
  case ets:info(kapok_namespaces) of
    undefined -> init_namespace_table();
    _ -> ok
  end,
  {_TClauses, TEnv} = translate_namespace_clauses(T, Env#{namespace := Id}),
  add_namespace({Id, maps:new(), maps:new(), sets:new()}),
  Line = ?line(Meta),
  {{attribute, Line, module, list_to_atom(Id)}, TEnv}.


%% Helper functions
translate_namespace_clauses(Clauses, Env) when is_list(Clauses) ->
  lists:mapfoldl(fun translate_namespace_clause/2, Env, Clauses).

translate_namespace_clause({list, _, [{identifier, _, "require"} | T]}, Env) ->
  {Names, TEnv} = handle_require_clause(T, Env),
  #{requires := Requires, module_aliases := ModuleAliases} = TEnv,
  io:format("all require: ~p~nall alias: ~p~n", [Requires, ModuleAliases]),
  {Names, TEnv};
translate_namespace_clause({list, _, [{identifier, _, "use"} | T]}, Env) ->
  {Names, TEnv} = handle_use_clause(T, Env),
  #{requires := Requires, module_aliases := ModuleAliases, functions := Functions} = TEnv,
  io:format("all require: ~p~nall alias: ~p~nall functions: ~p~n", [Requires, ModuleAliases, Functions]),
  {Names, TEnv}.

%% require
handle_require_clause(List, Env) when is_list(List) ->
  lists:mapfoldl(fun handle_require_element/2, Env, List).

handle_require_element({atom, Meta, Atom}, Env) ->
  Name = atom_to_list(Atom),
  {Name, kapok_env:add_require(Meta, Env, Name)};
handle_require_element({identifier, Meta, Id}, Env) ->
  {Id, kapok_env:add_require(Meta, Env, Id)};
handle_require_element({dot, Meta, List}, Env) ->
  Name = string:join(flatten_dot(List), "."),
  {Name, kapok_env:add_require(Meta, Env, Name)};
handle_require_element({ListType, Meta, Args}, Env) when ?is_list_type(ListType) ->
  case Args of
    [{atom, _, _} = Ast, {atom, _, 'as'}, {identifier, _, Id}] ->
      {Name, TEnv} = handle_require_element(Ast, Env),
      {Name, kapok_env:add_alias(Meta, TEnv, Id, Name)};
    [{identifier, _, _} = Ast, {atom, _, 'as'}, {identifier, _, Id}] ->
      {Name, TEnv} = handle_require_element(Ast, Env),
      {Name, kapok_env:add_alias(Meta, TEnv, Id, Name)};
    [{dot, _, _} = Ast, {atom, _, 'as'}, {identifier, _, Id}] ->
      {Name, TEnv} = handle_require_element(Ast, Env),
      {Name, kapok_env:add_alias(Meta, TEnv, Id, Name)};
    _ ->
      kapok_error:compile_error(Meta, ?m(Env, file), "invalid require expression ~p", [Args])
  end.

flatten_dot(List) ->
 flatten_dot(List, []).
flatten_dot([{dot, _, List}, {identifier, _, Id}], Acc) ->
  flatten_dot(List, [Id | Acc]);
flatten_dot([{identifier, _, Id1}, {identifier, _, Id2}], Acc) ->
  [Id1, Id2 | Acc].

%% use
handle_use_clause(List, Env) when is_list(List) ->
  lists:mapfoldl(fun handle_use_element/2, Env, List).

handle_use_element({atom, Meta, Atom}, Env) ->
  NewEnv = add_module_exports(Meta, Atom, Env),
  Name = atom_to_list(Atom),
  {Name, kapok_env:add_require(Meta, NewEnv, Name)};
handle_use_element({identifier, Meta, Id}, Env) ->
  NewEnv = add_module_exports(Meta, Id, Env),
  {Id, kapok_env:add_require(Meta, NewEnv, Id)};
handle_use_element({dot, Meta, Args}, Env) ->
  Name = string:join(flatten_dot(Args), "."),
  NewEnv = add_module_exports(Meta, Name, Env),
  {Name, kapok_env:add_require(Meta, NewEnv, Name)};
handle_use_element({ListType, Meta, Args}, Env) when ?is_list_type(ListType) ->
  case Args of
    [{atom, _, _} = Ast | T] ->
      {Name, TEnv} = handle_require_element(Ast, Env),
      handle_use_element_list(Meta, Name, T, TEnv);
    [{identifier, _, _} = Ast | T] ->
      {Name, TEnv} = handle_require_element(Ast, Env),
      handle_use_element_list(Meta, Name, T, TEnv);
    [{dot, _, _} = Ast | T] ->
      {Name, TEnv} = handle_require_element(Ast, Env),
      handle_use_element_list(Meta, Name, T, TEnv);
    _ ->
      kapok_error:compile_error(Meta, ?m(Env, file), "invalid use expression ~p", [Args])
  end.

handle_use_element_list(_Meta, Name, [], Env) ->
  {Name, Env};
handle_use_element_list(Meta, Name, [{atom, _, 'as'}, {identifier, _, Id} | T], Env) ->
  handle_use_element_list(Meta, Name, T, kapok_env:add_alias(Meta, Env, Id, Name));
handle_use_element_list(Meta, Name, [{atom, _, 'only'}, {ListType, _, Args}], Env)
    when ?is_list_type(ListType) ->
  Functions = filter_exports(Meta, Name, Args, Env),
  {Name, kapok_env:add_functions(Meta, Env, Functions)};
handle_use_element_list(Meta, Name, [{atom, _, 'exclude', {ListType, _, Args}}], Env)
    when ?is_list_type(ListType) ->
  Functions = filter_out_exports(Meta, Name, Args, Env),
  {Name, kapok_env:add_functions(Meta, Env, Functions)}.

add_module_exports(Meta, Module, Env) ->
  ensure_loaded(Meta, Module, Env),
  Functions = get_exports(Meta, Module, Env),
  kapok_env:add_functions(Meta, Env, Functions).

ensure_loaded(Meta, Module, Env) when is_list(Module) ->
  ensure_loaded(Meta, list_to_atom(Module), Env);
ensure_loaded(Meta, Module, Env) when is_atom(Module) ->
  case code:ensure_loaded(Module) of
    {module, Module} ->
      ok;
    {error, What} ->
      kapok_error:compile_error(Meta, ?m(Env, file), "fail to load module: ~p due to load error: ~p", [Module, What])
  end.

get_exports(Meta, Module, Env) when is_list(Module) ->
  get_exports(Meta, list_to_atom(Module), Env);
get_exports(Meta, Module, Env) when is_atom(Module) ->
  try
    Exports = Module:module_info(exports),
    orddict:from_list(lists:map(fun(E) -> {E, {Module, E}} end, Exports))
  catch
    error:undef ->
      kapok_error:compile_error(Meta, ?m(Env, file), "fail to get exports for unloaded module: ~p", [Module])
  end.

filter_exports(Meta, Module, Args, Env) ->
  Exports = get_exports(Meta, Module, Env),
  ToFilter = get_functions(Module, Args),
  Absent = orddict:filter(fun(K, _) -> orddict:is_key(K, Exports) == false end, ToFilter),
  case orddict:size(Absent) of
    0 -> ToFilter;
    _ -> kapok_error:compile_error(Meta, ?m(Env, file), "module ~p has no exported function: ~p", [Module, Absent])
  end.

filter_out_exports(Meta, Module, Args, Env) ->
  Exports = get_exports(Meta, Module, Env),
  ToFilterOut = get_functions(Module, Args),
  Absent = orddict:filter(fun(K, _) -> orddict:is_key(K, Exports) == false end, ToFilterOut),
  case orddict:size(Absent) of
    [] -> ok;
    _ -> kapok_error:compile_error(Meta, ?m(Env, file), "module ~p has no exported function: ~p", [Module, Absent])
  end,
  orddict:filter(fun(K, _) -> orddict:is_key(K, ToFilterOut) == false end, Exports).

get_functions(Module, Args) ->
  L = get_function_id_list(Args),
  orddict:from_list(lists:map(fun(E) -> {E, {Module, E}} end, L)).

get_function_id_list(Args) when is_list(Args) ->
  get_function_id_list(Args, []).
get_function_id_list([], Acc) ->
  lists:reverse(Acc);
get_function_id_list([{function_id, _, {{identifier, _, Id}, {integer, _, Integer}}} | Left], Acc) ->
  F = {list_to_atom(Id), Integer},
  get_function_id_list(Left, [F | Acc]).

