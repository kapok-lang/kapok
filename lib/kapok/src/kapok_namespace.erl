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
  Name = list_to_atom(Id),
  case ets:info(kapok_namespaces) of
    undefined -> init_namespace_table();
    _ -> ok
  end,
  {_TClauses, TEnv} = translate_namespace_clauses(T, Env#{namespace := Name}),
  add_namespace({Name, maps:new(), maps:new(), sets:new()}),
  Line = ?line(Meta),
  {{attribute, Line, module, Name}, TEnv}.


%% Helper functions
translate_namespace_clauses(Clauses, Env) when is_list(Clauses) ->
  lists:mapfoldl(fun translate_namespace_clause/2, Env, Clauses).

translate_namespace_clause({list, _, [{identifier, _, "require"} | T]}, Env) ->
  {Names, TEnv} = handle_require_clause(T, Env),
  #{requires := Requires, aliases := Aliases} = TEnv,
  io:format("all require: ~p~nall alias: ~p~n", [Requires, Aliases]),
  {Names, TEnv};
translate_namespace_clause({list, _, [{identifier, _, "use"} | T]}, Env) ->
  {Names, TEnv} = handle_use_clause(T, Env),
  #{uses := Uses, aliases := Aliases} = TEnv,
  io:format("all use: ~p~nall alias: ~p~n", [Uses, Aliases]),
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
handle_require_element({ListType, Meta, List}, Env)
    when ListType == list; ListType == literal_list ->
  case List of
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
      kapok_error:compile_error(Meta, ?m(Env, file), "invalid require expression ~p", [List])
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
  NewEnv = add_module_exports(Meta, list_to_atom(Id), Env),
  {Id, kapok_env:add_require(Meta, NewEnv, Id)};
handle_use_element({dot, Meta, List}, Env) ->
  Name = string:join(flatten_dot(List), "."),
  NewEnv = add_module_exports(Meta, list_to_atom(Name), Env),
  {Name, kapok_env:add_require(Meta, NewEnv, Name)}.

add_module_exports(Meta, Module, Env) when is_atom(Module) ->
  ensure_loaded(Meta, Module, Env),
  Exports = get_exports(Meta, Module, Env),
  kapok_env:add_functions(Env, Exports).

ensure_loaded(Meta, Module, Env) when is_atom(Module) ->
  case code:ensure_loaded(Module) of
    {module, Module} ->
      ok;
    {error, What} ->
      kapok_error:compile_error(Meta, ?m(Env, file), "fail to load module: ~p due to load error: ~p", [What])
  end.

get_exports(Meta, Module, Env) ->
  try
    Module:module_info()
  catch
    error:undef ->
      kapok_error:compile_error(Meta, ?m(Env, file), "fail to get exports for unloaded module: ~p", [Module])
  end.

