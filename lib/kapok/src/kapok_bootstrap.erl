%% An Erlang module that behaves like an Kapok module used for bootstraping.
-module(kapok_bootstrap).
-export(['MACRO-ns'/3,
         'MACRO-defn'/4,
         'MACRO-defn-'/4,
         'MACRO-defmacro'/4,
         '__info__'/1]).
-include("kapok.hrl").

'MACRO-ns'({Line, Env}, Call, Exprs) ->
  %% expand ns macro here.
  {ECall, EEnv} = kapok_expand:expand_all(Call, Env),
  {EExprs, EEnv1} = kapok_expand:expand_all(Exprs, EEnv),
  {_, EEnv2} = handle_ns(Line, EEnv1, ECall, EExprs),
  io:format("----- env: ~p~n", [EEnv2]),
  {{ns, [{line, Line}], maps:get(namespace, EEnv2)}, EEnv2}.

'MACRO-defn'(Caller, Name, Args, Exprs) ->
  define(Caller, 'defn', Name, Args, Exprs).

'MACRO-defn-'(Caller, Name, Args, Exprs) ->
  define(Caller, 'defn-', Name, Args, Exprs).

'MACRO-defmacro'(Caller, Name, Args, Exprs) ->
  define(Caller, 'defmacro', Name, Args, Exprs).

'__info__'(functions) ->
  [];
'__info__'(macros) ->
  [{'MACRO-defn', 3, rest},
   {'MACRO-defn-', 3, rest},
   {'MACRO-defmacro', 3, rest},
   {'MACRO-ns', 2, rest}].

handle_ns(Line, Env, {identifier, _, Id}, Exprs) ->
  E = case ?m(Env, namespace) of
        nil -> Env#{namespace => Id};
        _ -> kapok_env:env_for_eval([{file, ?m(Env, file)}, {namespace, Id}])
      end,
  handle_ns(Line, E, Exprs).

handle_ns(_Line, Env, Clauses) when is_list(Clauses) ->
  lists:mapfoldl(fun handle_namespace_clause/2, Env, Clauses).

handle_namespace_clause({list, _, [{identifier, _, 'require'} | T]}, Env) ->
  {Names, TEnv} = handle_require_clause(T, Env),
  #{requires := Requires} = TEnv,
  io:format("<<< after handle require, return >>>~n"),
  io:format("require: ~p~n", [Requires]),
  {Names, TEnv};
handle_namespace_clause({list, _, [{identifier, _, 'use'} | T]}, Env) ->
  {Names, TEnv} = handle_use_clause(T, Env),
  #{requires := Requires,
    functions := Functions} = TEnv,
  io:format("<<< after handle ns, return >>>~n"),
  io:format("require: ~p~nfunctions: ~p~n", [Requires, Functions]),
  {Names, TEnv}.

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

define({Line, Env}, Kind, Name, Args, Exprs) ->
  QuotedName = kapok_macro:quote(Name),
  QuotedArgs = kapok_macro:quote(Args),
  QuotedExprs = kapok_macro:quote(Exprs),
  QuotedEnv = kapok_macro:quote(Env),
  ArgList = [{integer, [], Line}, {atom, [], Kind}, QuotedName, QuotedArgs, QuotedExprs, QuotedEnv],
  {{list, [], [{dot, [], {'kapok_def', 'store_definition'}} | ArgList]}, Env}.

