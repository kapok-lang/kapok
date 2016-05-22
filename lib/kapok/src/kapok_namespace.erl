%% namespace
-module(kapok_namespace).
-export([compile_namespace/3,
         format_error/1
        ]).
-include("kapok.hrl").

init_namespace_table() ->
  _ = ets:new(kapok_namespaces, [set, protected, named_table, {read_concurrency, true}]).

add_namespace(Namespace) ->
  ets:insert(kapok_namespaces, {Namespace, [], [], []}).

add_function_clause(Namespace, Fun, Arity, Clause) ->
  add_clause(Namespace, 'fun', Fun, Arity, Clause).

add_clause(Namespace, Kind, Fun, Arity, Clause) ->
  Index = case Kind of
            'fun' -> 2;
            'macro' -> 3
          end,
  Old = ets:lookup_element(kapok_namespaces, Namespace, Index),
  C = case orddict:find({Fun, Arity}, Old) of
        {ok, Clauses} -> [Clause|Clauses];
        error -> [Clause]
      end,
  New = orddict:store({Fun, Arity}, C, Old),
  ets:update_element(kapok_namespaces, Namespace, {Index, New}).


namespace_functions(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 2).

add_macro_clause(Namespace, Fun, Arity, Clause) ->
  add_clause(Namespace, 'macro', Fun, Arity, Clause).

namespace_macros(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 3).

add_export(Namespace, Fun, Arity) ->
  OldExports = namespace_exports(Namespace),
  NewExports = ordsets:add_element({Fun, Arity}, OldExports),
  ets:update_element(kapok_namespaces, Namespace, {4, NewExports}).

namespace_exports(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 4).


compile_namespace(Ast, Env, Dest) when is_list(Ast) ->
  EEnv = lists:foldl(fun (A, E) ->
                         {EA, EE} = kapok_expand:expand_all(A, E),
                         TE = handle_ast(EA, EE),
                         {EA, TE}
                     end,
                     Env,
                     Ast),
  %% for A in Ast:
  %%   {EA, EEnv} = expand_all(A),
  %%   case EA of
  %%     (ns) -> handle ns, only one ns statement is good
  %%     def -> compile_def()
  %%     Other -> unsupported other
  %%
  %% get def table of namespace, compile it
  %% call namespace.main() if (there is main exported, and, in script mode)
  %%
  {}.

handle_ast({list, Meta, [{identifier, _, ns}, {identifier, _, Id}, {StringType, _, Doc} | Left]}, Env)
    when ?is_string_type(StringType) ->
  handle_ns(Meta, Id, Doc, Left, Env);
handle_ast({list, Meta, [{identifier, _, ns}, {identifier, _, Id} | Left]}, Env) ->
  handle_ns(Meta, Id, Left, Env);
handle_ast({list, Meta, [{identifier, _, Id}, {identifier, _, Name}, {ListType, _, _} = Args, {StringType, _, Doc} | Body]}, Env) when ?is_def(Id), ?is_list_type(ListType), ?is_string_type(StringType) ->
  handle_def(Meta, Kind, Name, Args, Doc, Body, Env);
handle_ast({list, Meta, [{identifier, _, Id}, {identifier, _, Name}, {ListType, _, _} = Args | Body]}, Env) when ?is_def(Id), ?is_list_type(ListType) ->
  handle_def(Meta, Kind, Name, Args, Body, Env);

handle_ast({_, Meta, _} = Ast, Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {invalid_expression, {Ast}}).

%% namespace

handle_ns(Meta, Name, Clauses, Env) ->
  handle_ns(Meta, Name, Clauses, <<"">>, Env).
handle_ns(Meta, Name, Clauses, _Doc, Env) ->
  E = case ?m(Env, namespace) of
        nil -> Env#{namespace => Name};
        NS -> kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {multiple_namespace, {NS, Name}})
      end,
  lists:mapfoldl(fun handle_namespace_clause/2, E, Clauses).

handle_namespace_clause({list, _, [{identifier, _, 'require'} | T]}, Env) ->
  {_Names, TEnv} = handle_require_clause(T, Env),
  #{requires := Requires} = TEnv,
  io:format("<<< after handle require, return >>>~n"),
  io:format("require: ~p~n", [Requires]),
  TEnv;
handle_namespace_clause({list, _, [{identifier, _, 'use'} | T]}, Env) ->
  {_Names, TEnv} = handle_use_clause(T, Env),
  #{requires := Requires,
    functions := Functions} = TEnv,
  io:format("<<< after handle ns, return >>>~n"),
  io:format("require: ~p~nfunctions: ~p~n", [Requires, Functions]),
  TEnv.

%% require
handle_require_clause(List, Env) when is_list(List) ->
  lists:mapfoldl(fun handle_require_element/2, Env, List).

handle_require_element({atom, Meta, Atom}, Env) ->
  {Atom, kapok_env:add_require(Meta, Env, Atom)};
handle_require_element({identifier, Meta, Id}, Env) ->
  {Id, kapok_env:add_require(Meta, Env, Id)};
handle_require_element({dot, Meta, _} = Dot, Env) ->
  Name = kapok_parser:dot_fullname(Dot),
  {Name, kapok_env:add_require(Meta, Env, Name)};
handle_require_element({ListType, Meta, Args}, Env) when ?is_list_type(ListType) ->
  case Args of
    [{atom, _, _} = Ast, {atom, _, 'as'}, {identifier, _, Id}] ->
      {Name, TEnv} = handle_require_element(Ast, Env),
      {Name, kapok_env:add_require(Meta, TEnv, Id, Name)};
    [{identifier, _, _} = Ast, {atom, _, 'as'}, {identifier, _, Id}] ->
      {Name, TEnv} = handle_require_element(Ast, Env),
      {Name, kapok_env:add_require(Meta, TEnv, Id, Name)};
    [{dot, _, _} = Ast, {atom, _, 'as'}, {identifier, _, Id}] ->
      {Name, TEnv} = handle_require_element(Ast, Env),
      {Name, kapok_env:add_require(Meta, TEnv, Id, Name)};
    _ ->
      kapok_error:compile_error(Meta, ?m(Env, file), "invalid require expression ~p", [Args])
  end.

%% use
handle_use_clause(List, Env) when is_list(List) ->
  lists:mapfoldl(fun handle_use_element/2, Env, List).

handle_use_element({atom, Meta, Atom}, Env) ->
  NewEnv = add_module_exports(Meta, Atom, Env),
  {Atom, kapok_env:add_require(Meta, NewEnv, Atom)};
handle_use_element({identifier, Meta, Id}, Env) ->
  NewEnv = add_module_exports(Meta, Id, Env),
  {Id, kapok_env:add_require(Meta, NewEnv, Id)};
handle_use_element({dot, Meta, _} = Dot, Env) ->
  Name = kapok_parser:dot_fullname(Dot),
  NewEnv = add_module_exports(Meta, Name, Env),
  {Name, kapok_env:add_require(Meta, NewEnv, Name)};
handle_use_element({ListType, Meta, Args}, Env) when ?is_list_type(ListType) ->
  case Args of
    [{atom, _, _} = Ast | T] ->
      {Name, TEnv} = handle_require_element(Ast, Env),
      handle_use_element_arguments(Meta, Name, T, TEnv);
    [{identifier, _, _} = Ast | T] ->
      {Name, TEnv} = handle_require_element(Ast, Env),
      handle_use_element_arguments(Meta, Name, T, TEnv);
    [{dot, _, _} = Ast | T] ->
      {Name, TEnv} = handle_require_element(Ast, Env),
      handle_use_element_arguments(Meta, Name, T, TEnv);
    _ ->
      kapok_error:compile_error(Meta, ?m(Env, file), "invalid use expression ~p", [Args])
  end.

handle_use_element_arguments(Meta, Name, Args, Env) ->
  GArgs = group_arguments(Meta, Args, Env),
  handle_use_element_arguments(Meta, Name, nil, GArgs, Env).
handle_use_element_arguments(_Meta, Name, _, [], Env) ->
  {Name, Env};
handle_use_element_arguments(Meta, Name, Meta1, [{{atom, _, 'as'}, {identifier, _, Id}} | T], Env) ->
  handle_use_element_arguments(Meta, Name, Meta1, T, kapok_env:add_require(Meta, Env, Id, Name));
handle_use_element_arguments(Meta, Name, _, [{{atom, Meta1, 'exclude'}, {_, _, Args}} | T], Env) ->
  Functions = filter_out_exports(Meta, Name, Args, Env),
  NewEnv = kapok_env:add_function(Meta, Env, Name, Functions),
  handle_use_element_arguments(Meta, Name, Meta1, T, NewEnv);
handle_use_element_arguments(Meta, Name, nil, [{{atom, _, 'only'}, {_, _, Args}} | T], Env) ->
  Functions = filter_exports(Meta, Name, Args, Env),
  NewEnv = kapok_env:add_function(Meta, Env, Name, Functions),
  handle_use_element_arguments(Meta, Name, nil, T, NewEnv);
handle_use_element_arguments(_Meta, _Name, Meta1, [{{atom, Meta2, 'only'}, {_, _, _}} | _T], Env) ->
  kapok_error:compile_error(Meta2, ?m(Env, file), "invalid usage of :only with :exclude present at line: ~p", [?line(Meta1)]);
handle_use_element_arguments(Meta, Name, Meta1, [{{atom, _, 'rename'}, {_, _, Args}} | T], Env) ->
  Aliases = get_function_aliases(Meta, Args, Env),
  NewEnv = kapok_env:add_function(Meta, Env, Name, Aliases),
  handle_use_element_arguments(Meta, Name, Meta1, T, NewEnv).

%% definitions
handle_def(Meta, Kind, Name, Args, Body, Env) ->
  handle_def(Meta, Kind, Name, Args, <<"">>, Body, Env).
handle_def(Meta, Kind, Name, Args, _Doc, Body, Env) ->
  %% TODO add doc
  Env1 = kapok_env:push_scope(Env),
  %% TODO add vars from args to env.scope
  {TArgs, TEnv1} = kapok_translate:translate(Args, Env1),
  {TBody, TEnv2} = kapok_translate:translate(Body, TEnv1),
  Arity = length(TArgs),
  Namespace = ?m(Env, namespace),
  case Kind of
    'defn' ->
      add_function_clause(Namespace, Name, Arity, {clause, ?line(Meta), TArgs, [], TBody}),
      add_export(Namespace, Name, Arity);
    'defn-' ->
      add_function_clause(Namespace, Name, Arity, {clause, ?line(Meta), TArgs, [], TBody});
    'defmacro' ->
      add_macro_clause(Namespace, Name, Arity, {clause, ?line(Meta), TArgs, [], TBody}),
      add_export(Namespace, Name, Arity)
  end,
  kapok_env:pop_scope(TEnv2).


%% Helpers

group_arguments(Meta, Args, Env) ->
  group_arguments(Meta, Args, orddict:new(), Env).
group_arguments(_Meta, [], Acc, _Env) ->
  lists:map(fun ({_, KV}) -> KV end, orddict:to_list(Acc));
group_arguments(Meta, [{atom, _, 'as'} = K, {identifier, _, _} = V | T], Acc, Env) ->
  group_arguments(Meta, T, orddict:store('as', {K, V}, Acc), Env);
group_arguments(Meta, [{atom, _, 'only'} = K, {ListType, _, _} = V | T], Acc, Env)
    when ?is_list_type(ListType) ->
  group_arguments(Meta, T, orddict:store('only', {K, V}, Acc), Env);
group_arguments(Meta, [{atom, _, 'exclude'} = K, {ListType, _, _} = V | T], Acc, Env)
    when ?is_list_type(ListType) ->
  group_arguments(Meta, T, orddict:store('exclude', {K, V}, Acc), Env);
group_arguments(Meta, [{atom, _, 'rename'} = K, {ListType, _, _} = V | T], Acc, Env)
    when ?is_list_type(ListType) ->
  group_arguments(Meta, T, orddict:store('rename', {K, V}, Acc), Env);
group_arguments(Meta, Args, _Acc, Env) ->
  kapok_error:compile_error(Meta, ?m(Env, file), "invalid use arguments: ~p~n", [Args]).

add_module_exports(Meta, Module, Env) ->
  ensure_loaded(Meta, Module, Env),
  Functions = get_exports(Meta, Module, Env),
  kapok_env:add_function(Meta, Env, Module, Functions).

ensure_loaded(Meta, Module, Env) ->
  case code:ensure_loaded(Module) of
    {module, Module} ->
      ok;
    {error, What} ->
      kapok_error:compile_error(Meta, ?m(Env, file), "fail to load module: ~p due to load error: ~p", [Module, What])
  end.

get_exports(Meta, Module, Env) ->
  try
    L = Module:module_info(exports),
    ordsets:from_list(L)
  catch
    error:undef ->
      kapok_error:compile_error(Meta, ?m(Env, file), "fail to get exports for unloaded module: ~p", [Module])
  end.

filter_exports(Meta, Module, Args, Env) ->
  Exports = get_exports(Meta, Module, Env),
  ToFilter = get_functions(Meta, Args, Env),
  %% TODO filter macro definitions
  case ordsets:subtract(ToFilter, Exports) of
    [] -> ToFilter;
    Absent -> kapok_error:compile_error(Meta, ?m(Env, file), "module ~p has no exported function: ~p", [Module, Absent])
  end.

filter_out_exports(Meta, Module, Args, Env) ->
  Exports = get_exports(Meta, Module, Env),
  ToFilterOut = get_functions(Meta, Args, Env),
  %% TODO filter out macro definitions
  case ordsets:subtract(ToFilterOut, Exports) of
    [] -> ok;
    Absent -> kapok_error:compile_error(Meta, ?m(Env, file), "module ~p has no exported function: ~p", [Module, Absent])
  end,
  ordsets:subtract(Exports, ToFilterOut).

get_functions(Meta, Args, Env) ->
  L = lists:map(fun ({function_id, _, {{identifier, _, Id}, {integer, _, Integer}}}) ->
                    {Id, Integer};
                    (Other) ->
                    kapok_error:compile_error(Meta, ?m(Env, file), "invalid function id: ~p", [Other])
                end,
                Args),
  ordsets:from_list(lists:reverse(L)).

get_function_aliases(Meta, Args, Env) ->
  L = lists:map(fun ({ListType, _, [{function_id, _, {{identifier, _, OriginalId}, {integer, _, Integer}}},
                                    {identifier, _, NewId}]}) when ?is_list_type(ListType) ->
                    {NewId, {OriginalId, Integer}};
                    (Other) ->
                    kapok_error:compile_error(Meta, ?m(Env, file), "invalid rename arguments: ~p", [Other])
                end,
                Args),
  ordsets:from_list(L).


%% Error

format_error({invalid_expression, {Ast}}) ->
  io_lib:format("invalid expression ~p", [Ast]);
format_error({multiple_namespace, {Existing, New}}) ->
  io_lib:format("multiple namespace in one file not supported: ~p, ~p", [Existing, New]).

