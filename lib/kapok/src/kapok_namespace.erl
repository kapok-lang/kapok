%% namespace
-module(kapok_namespace).
-export([compile/3,
         format_error/1,
         namespace_exports/1,
         namespace_export_functions/1,
         namespace_export_macros/1
        ]).
-include("kapok.hrl").

init_namespace_table() ->
  _ = ets:new(kapok_namespaces, [set, protected, named_table, {read_concurrency, true}]).

add_namespace(Namespace) ->
  ets:insert(kapok_namespaces, {Namespace, [], [], [], []}).

add_function_clause(Namespace, Fun, Arity, Clause) ->
  add_clause(Namespace, 'fun', Fun, Arity, Clause).

add_clause(Namespace, Kind, Fun, Arity, Clause) ->
  Index = case Kind of
            'fun' -> 2;
            'macro' -> 3
          end,
  Old = ets:lookup_element(kapok_namespaces, Namespace, Index),
  Key = {Fun, Arity},
  C = case orddict:find(Key, Old) of
        {ok, Clauses} -> [Clause|Clauses];
        error -> [Clause]
      end,
  New = orddict:store(Key, C, Old),
  ets:update_element(kapok_namespaces, Namespace, {Index, New}).


namespace_functions(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 2).

add_macro_clause(Namespace, Fun, Arity, Clause) ->
  add_clause(Namespace, 'macro', Fun, Arity, Clause).

namespace_macros(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 3).

namespace_defs(Namespace) ->
  Functions = namespace_functions(Namespace),
  Macros = namespace_macros(Namespace),
  %% TODO macro will shadow function if there is any overrides
  orddict:merge(fun(_K, _V1, V2) -> V2 end, Functions, Macros).

namespace_export_functions(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 4).

add_export_function(Namespace, Fun, Arity, ParameterType) ->
  add_export(Namespace, 'fun', Fun, Arity, ParameterType).

namespace_export_macros(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 5).

add_export_macro(Namespace, Macro, Arity, ParameterType) ->
  add_export(Namespace, 'macro', Macro, Arity, ParameterType).

add_export(Namespace, Kind, F, A, ParameterType) ->
  OldExports = case Kind of
                 'fun' -> namespace_export_functions(Namespace);
                 'macro' -> namespace_export_macros(Namespace)
               end,
  NewExports = ordsets:add_element({F, A, ParameterType}, OldExports),
  Index = case Kind of
            'fun' -> 4;
            'macro' -> 5
          end,
  ets:update_element(kapok_namespaces, Namespace, {Index, NewExports}).

namespace_exports(Namespace) ->
  Functions = namespace_export_functions(Namespace),
  Macros = namespace_export_macros(Namespace),
  ordsets:union(Functions, Macros).

module_exports(Functions, Macros) ->
  ExportFunctions = [{F, A} || {F, A, _P} <- Functions],
  ExportMacros = [{kapok_utils:macro_name(F), A} || {F, A, _P} <- Macros],
  io:format("export macros: ~p~n", [ExportMacros]),
  %% TODO macro will shadow function if there is any overrides
  ordsets:union(ExportFunctions, ExportMacros).

info_fun(Functions, Macros, Env) ->
  {FunctionList, _} = kapok_translate:translate(kapok_expand:quote(Functions), Env),
  {MacroList, _} = kapok_translate:translate(kapok_expand:quote(Macros), Env),
  {function, 0,'__info__',1,
   [{clause,0,[{atom,0,'functions'}],[],[FunctionList]},
    {clause,0,[{atom,0,'macros'}],[],[MacroList]}]}.

compile(Ast, Env, Callback) when is_list(Ast) ->
  TEnv = lists:foldl(fun(A, E) ->
                         {EA, EE} = kapok_expand:expand_all(A, E),
                         handle_ast(EA, EE)
                     end,
                     Env,
                     Ast),
  Namespace = ?m(TEnv,namespace),
  Functions = namespace_export_functions(Namespace),
  Macros = namespace_export_macros(Namespace),
  Defs = namespace_defs(Namespace),
  build_module(Namespace, Functions, Macros, Defs, TEnv, Callback).

handle_ast({list, Meta, [{identifier, _, ns}, {identifier, _, Id}, {C, _, Doc} | Left]}, Env)
    when ?is_string(C) ->
  handle_ns(Meta, Id, Doc, Left, Env);
handle_ast({list, Meta, [{identifier, _, ns}, {identifier, _, Id} | Left]}, Env) ->
  handle_ns(Meta, Id, Left, Env);
handle_ast({list, Meta, [{identifier, _, Id}, {identifier, _, Name}, {C1, _, _} = Args, {C2, _, _} = Doc | Body]}, Env)
    when ?is_def(Id), ?is_arg_list(C1), ?is_string(C2) ->
  handle_def(Meta, Id, Name, Args, Doc, Body, Env);
handle_ast({list, Meta, [{identifier, _, Id}, {identifier, _, Name}, {C, _, _} = Args | Body]}, Env)
    when ?is_def(Id), ?is_arg_list(C) ->
  handle_def(Meta, Id, Name, Args, Body, Env);

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
  case ets:info(kapok_namespaces) of
    undefined -> init_namespace_table();
    _ -> ok
  end,
  add_namespace(Name),
  {_, TEnv} = lists:mapfoldl(fun handle_namespace_clause/2, E, Clauses),
  TEnv.

handle_namespace_clause({list, _, [{identifier, _, 'require'} | T]}, Env) ->
  handle_require_clause(T, Env);
handle_namespace_clause({list, _, [{identifier, _, 'use'} | T]}, Env) ->
  handle_use_clause(T, Env).

%% require
handle_require_clause(List, Env) when is_list(List) ->
  lists:mapfoldl(fun handle_require_element/2, Env, List).

handle_require_element({Category, Meta, Id}, Env) when ?is_id(Category) ->
  {Id, kapok_env:add_require(Meta, Env, Id)};
handle_require_element({dot, Meta, _} = Dot, Env) ->
  Name = kapok_parser:dot_fullname(Dot),
  {Name, kapok_env:add_require(Meta, Env, Name)};
handle_require_element({Category, Meta, Args}, Env) when ?is_list(Category) ->
  case Args of
    [{Category, _, _} = Ast, {atom, _, 'as'}, {identifier, _, Id}] when ?is_id(Category) ->
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

handle_use_element({Category, Meta, Id}, Env) when ?is_id(Category) ->
  Env1 = kapok_env:add_require(Meta, Env, Id),
  Env2 = kapok_env:add_use(Meta, Env1, Id),
  {Id, Env2};
handle_use_element({dot, Meta, _} = Dot, Env) ->
  Name = kapok_parser:dot_fullname(Dot),
  Env1 = kapok_env:add_require(Meta, Env, Name),
  Env2 = kapok_env:add_use(Meta, Env1, Name),
  {Name, Env2};
handle_use_element({Category, Meta, Args}, Env) when ?is_list(Category) ->
  case Args of
    [{C, _, _} = Ast | T] when ?is_id(C) ->
      {Name, Env1} = handle_use_element(Ast, Env),
      handle_use_element_arguments(Meta, Name, T, Env1);
    [{dot, _, _} = Ast | T] ->
      {Name, Env1} = handle_use_element(Ast, Env),
      handle_use_element_arguments(Meta, Name, T, Env1);
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
handle_use_element_arguments(Meta, Name, _, [{{atom, Meta1, 'exclude'}, {_, Meta2, Args}} | T], Env) ->
  Functions = parse_functions(Meta2, Args, Env),
  Env1 = kapok_env:add_use(Meta1, Env, Name, 'exclude', Functions),
  handle_use_element_arguments(Meta, Name, Meta1, T, Env1);
handle_use_element_arguments(Meta, Name, nil, [{{atom, Meta1, 'only'}, {_, Meta2, Args}} | T], Env) ->
  Functions = parse_functions(Meta2, Args, Env),
  Env1 = kapok_env:add_use(Meta1, Env, Name, 'only', Functions),
  handle_use_element_arguments(Meta, Name, nil, T, Env1);
handle_use_element_arguments(_Meta, _Name, Meta1, [{{atom, Meta2, 'only'}, {_, _, _}} | _T], Env) ->
  kapok_error:compile_error(Meta2, ?m(Env, file), "invalid usage of :only with :exclude present at line: ~p", [?line(Meta1)]);
handle_use_element_arguments(Meta, Name, Meta1, [{{atom, _, 'rename'}, {_, _, Args}} | T], Env) ->
  Aliases = parse_function_aliases(Meta, Args, Env),
  NewEnv = kapok_env:add_use(Meta, Env, Name, 'rename', Aliases),
  handle_use_element_arguments(Meta, Name, Meta1, T, NewEnv);
handle_use_element_arguments(_Meta, _Name, _, [{_, Meta1, _} = Ast | _T], Env) ->
  kapok_error:compile_error(Meta1, ?m(Env, file), "invalid use argument ~p", [Ast]).


%% definitions
handle_def(Meta, Kind, Name, Args, Body, Env) ->
  handle_def(Meta, Kind, Name, Args, <<"">>, Body, Env).
handle_def(Meta, Kind, Name, {Category, _, _} = Args, _Doc, Body, Env) ->
  %% TODO add doc
  Env1 = kapok_env:push_scope(Env),
  Arity = arg_length(Args),
  Namespace = ?m(Env, namespace),
  ParameterType = case Category of
                    C when ?is_list(C) -> 'normal';
                    C when ?is_cons_list(C) -> 'rest'
                  end,
  case Kind of
    'defmacro' ->
      TEnv3 = handle_macro_def(Meta, Namespace, Name, Arity, ParameterType, Args, Body, Env1);
    _ ->
      {TF, TEnv1} = kapok_translate:translate(Name, Env1),
      {TArgs, TEnv2} = kapok_translate:translate_args(Args, TEnv1),
      {TBody, TEnv3} = kapok_translate:translate(Body, TEnv2),
      add_function_clause(Namespace, Name, Arity, {clause, ?line(Meta), TArgs, [], TBody}),
      case Kind of
        'defn' -> add_export_function(Namespace, Name, Arity, ParameterType);
        'defn-' -> ok
      end,
      case ParameterType of
        'rest' -> add_rest_function_clause(Meta, Kind, Namespace, Name, Arity, TF, TArgs, TBody, TEnv3);
        _ -> ok
      end
  end,
  kapok_env:pop_scope(TEnv3).

%% module

build_module(Module, Functions, Macros, Defs, Env, Callback) ->
  Exports = module_exports(Functions, Macros),
  FunInfo = info_fun(Functions, Macros, Env),
  Funs = orddict:fold(fun({Fun, Arity}, Clauses, Acc) ->
                          [{function, 0, Fun, Arity, lists:reverse(Clauses)} | Acc]
                      end,
                      [],
                      Defs),
  AttrModule = {attribute,0,module,Module},
  AttrExport = {attribute,0,export,ordsets:to_list(Exports)},
  AttrInfo = {attribute,0,export,[{'__info__', 1}]},

  Erl = [AttrModule, AttrExport, AttrInfo, FunInfo | Funs],
  kapok_compiler:module(Erl, [], Env, Callback).

%% Helpers

group_arguments(Meta, Args, Env) ->
  group_arguments(Meta, Args, orddict:new(), Env).
group_arguments(_Meta, [], Acc, _Env) ->
  lists:map(fun({_, KV}) -> KV end, orddict:to_list(Acc));
group_arguments(Meta, [{atom, _, 'as'} = K, {identifier, _, _} = V | T], Acc, Env) ->
  group_arguments(Meta, T, orddict:store('as', {K, V}, Acc), Env);
group_arguments(Meta, [{atom, _, 'only'} = K, {C, _, _} = V | T], Acc, Env) when ?is_list(C) ->
  group_arguments(Meta, T, orddict:store('only', {K, V}, Acc), Env);
group_arguments(Meta, [{atom, _, 'exclude'} = K, {C, _, _} = V | T], Acc, Env) when ?is_list(C) ->
  group_arguments(Meta, T, orddict:store('exclude', {K, V}, Acc), Env);
group_arguments(Meta, [{atom, _, 'rename'} = K, {C, _, _} = V | T], Acc, Env) when ?is_list(C) ->
  group_arguments(Meta, T, orddict:store('rename', {K, V}, Acc), Env);
group_arguments(Meta, Args, _Acc, Env) ->
  kapok_error:compile_error(Meta, ?m(Env, file), "invalid use arguments: ~p~n", [Args]).

parse_functions(Meta, Args, Env) ->
  Fun = fun({function_id, _, {Id, Integer}}) -> {Id, Integer};
           ({Category, _, Id}) when ?is_id(Category) -> Id;
           (Other) -> kapok_error:compile_error(Meta, ?m(Env, file), "invalid function: ~p", [Other])
        end,
  ordsets:from_list(lists:map(Fun, Args)).


parse_function_aliases(Meta, Args, Env) ->
  Fun = fun({Category, _, [{function_id, _, {Id, Integer}}, {identifier, _, Alias}]}) when ?is_list(Category) ->
            {Alias, {Id, Integer}};
           ({Category, _, [{C1, _, Id}, {C2, _, Alias}]}) when ?is_list(Category), ?is_id(C1), ?is_id(C2) ->
            {Alias, Id};
           (Other) ->
            kapok_error:compile_error(Meta, ?m(Env, file), "invalid rename arguments: ~p", [Other])
        end,
  ordsets:from_list(lists:map(Fun, Args)).

arg_length({Category, _, Args}) when ?is_list(Category) ->
  length(Args);
arg_length({Category, _, {Head, _}}) when ?is_cons_list(Category) ->
  length(Head) + 1.


handle_macro_def(Meta, Namespace, Name, Arity, ParameterType, Args, Body, Env) ->
  MacroName = kapok_utils:macro_name(Name),
  MacroArity = Arity + 1,
  {TF, TEnv} = kapok_translate:translate(Name, Env),
  PrefixArgs = {tuple, Meta, [{identifier, Meta, line}, {identifier, Meta, env}]},
  TEnv1 = kapok_env:add_var(Meta, TEnv, line),
  TEnv2 = kapok_env:add_var(Meta, TEnv1, env),
  {TPrefixArgs, TEnv3} = kapok_translate:translate(PrefixArgs, TEnv2),
  {TArgs, TEnv4} = kapok_translate:translate_args(Args, TEnv3),
  MacroArgs = [TPrefixArgs | TArgs],
  {TBody, TEnv5} = kapok_translate:translate(Body, TEnv4),
  FunClause = {clause, ?line(Meta), TArgs, [], TBody},
  MacroClause = {clause, ?line(Meta), MacroArgs, [], [{call, ?line(Meta), TF, TArgs}]},
  add_function_clause(Namespace, Name, Arity, FunClause),
  add_macro_clause(Namespace, MacroName, MacroArity, MacroClause),
  add_export_macro(Namespace, Name, MacroArity, ParameterType),
  %% add rest call if necessary
  case ParameterType of
    'rest' ->
      RestFunArity = Arity-1,
      RestMacroArity = Arity,
      TNormalArgs = lists:sublist(TArgs, RestFunArity),
      {TListArgs, _} = kapok_translate:translate({literal_list, Meta, []}, Env),
      TRestArgs = TNormalArgs++[TListArgs],
      RestFunClause = {clause, ?line(Meta), TNormalArgs, [], [{call, ?line(Meta), TF, TRestArgs}]},
      RestMacroArgs = [TPrefixArgs | TRestArgs],
      RestMacroClause = {clause, ?line(Meta), RestMacroArgs, [], [{call, ?line(Meta), TF, TNormalArgs}]},
      add_function_clause(Namespace, Name, RestFunArity, RestFunClause),
      add_macro_clause(Namespace, MacroName, RestMacroArity, RestMacroClause),
      add_export_macro(Namespace, Name, RestMacroArity, 'normal');
    _ -> ok
  end,
  TEnv5.

add_rest_function_clause(Meta, Kind, Namespace, Name, Arity, TF, TArgs, TBody, Env) ->
  RestArity = Arity-1,
  TNormalArgs = lists:sublist(TArgs, RestArity),
  {TListArgs, _} = kapok_translate:translate({literal_list, 0, []}, Env),
  %% redirect normal call(without rest argument) to the clause with rest
  TRestArgs = TNormalArgs++[TListArgs],
  TBody = [{call, ?line(Meta), TF, TRestArgs}],
  add_function_clause(Namespace, Name, RestArity, {clause, ?line(Meta), TNormalArgs, [], TBody}),
  case Kind of
    'defn' -> add_export_function(Namespace, Name, RestArity, 'normal');
    _ -> ok
  end.


%% Error

format_error({invalid_expression, {Ast}}) ->
  io_lib:format("invalid expression ~p", [Ast]);
format_error({multiple_namespace, {Existing, New}}) ->
  io_lib:format("multiple namespace in one file not supported: ~p, ~p", [Existing, New]).

