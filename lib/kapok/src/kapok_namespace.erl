%% namespace
-module(kapok_namespace).
-export([compile/3,
         format_error/1,
         namespace_exports/1,
         namespace_locals/1,
         namespace_export_functions/1,
         namespace_export_macros/1
        ]).
-import(kapok_scanner, [token_text/1, token_meta/1]).
-include("kapok.hrl").

init_namespace_table() ->
  _ = ets:new(kapok_namespaces, [set, protected, named_table, {read_concurrency, true}]).

add_namespace(Namespace) ->
  ets:insert(kapok_namespaces, {Namespace, [], [], [], [], [], []}).

add_clause(Namespace, Kind, Fun, Arity, Clause) when Kind == 'defn' ->
  add_clause(Namespace, 2, Fun, Arity, Clause);
add_clause(Namespace, Kind, Fun, Arity, Clause) when Kind == 'defn-' ->
  add_clause(Namespace, 2, Fun, Arity, Clause);
add_clause(Namespace, Kind, Fun, Arity, Clause) when Kind == 'defmacro' ->
  add_clause(Namespace, 3, Fun, Arity, Clause);
add_clause(Namespace, Index, Fun, Arity, Clause) when is_integer(Index) ->
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

namespace_macros(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 3).

namespace_defs(Namespace) ->
  Functions = namespace_functions(Namespace),
  Macros = namespace_macros(Namespace),
  %% TODO macro will shadow function if there is any overrides
  orddict:merge(fun(_K, _V1, V2) -> V2 end, Functions, Macros).

namespace_export_functions(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 4).

namespace_export_macros(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 5).

add_export(_Namespace, Kind, _F, _A, _ParameterType) when Kind == 'defn-'; Kind == 'defalias-' ->
  %% don't export for namespace private function definitions
  ok;
add_export(Namespace, Kind, F, A, ParameterType) when Kind == 'defn'; Kind == 'defalias' ->
  add_export(Namespace, 4, F, A, ParameterType);
add_export(Namespace, Kind, F, A, ParameterType) when Kind == 'defmacro' ->
  add_export(Namespace, 5, F, A, ParameterType);
add_export(Namespace, Index, F, A, ParameterType) when is_integer(Index) ->
  OldExports = ets:lookup_element(kapok_namespaces, Namespace, Index),
  NewExports = ordsets:add_element({F, A, ParameterType}, OldExports),
  ets:update_element(kapok_namespaces, Namespace, {Index, NewExports}).

namespace_exports(Namespace) ->
  Functions = namespace_export_functions(Namespace),
  Macros = namespace_export_macros(Namespace),
  ordsets:union(Functions, Macros).

module_exports(Functions, Macros) ->
  GetExport = fun({F, A, _P}) -> {F, A} end,
  ExportFunctions = ordsets:from_list(lists:map(GetExport, Functions)),
  ExportMacros = ordsets:from_list(lists:map(GetExport, Macros)),
  %% TODO macro will shadow function if there is any overrides
  ordsets:union(ExportFunctions, ExportMacros).

namespace_aliases(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 6).

namespace_aliases(Namespace, Defs) ->
  Aliases = namespace_aliases(Namespace),
  lists:foldl(fun({Alias, F, A}, Acc) ->
                  {ok, Clauses} = orddict:find({F, A}, Defs),
                  orddict:store({Alias, A}, Clauses, Acc);
                 ({Alias, F}, Acc)->
                  L = orddict:filter(fun({F1, _A1}, _Clauses) -> F == F1 end, Defs),
                  orddict:fold(fun({_F, A}, Clauses, Acc1) ->
                                   orddict:store({Alias, A}, Clauses, Acc1)
                               end,
                               Acc,
                               L)
              end,
              orddict:new(),
              Aliases).

add_alias(Namespace, Alias, Original) ->
  Aliases = namespace_aliases(Namespace),
  NewAliases = case Original of
                 {Fun, Arity} -> ordsets:add_element({Alias, Fun, Arity}, Aliases);
                 Fun -> ordsets:add_element({Alias, Fun}, Aliases)
               end,
  ets:update_element(kapok_namespaces, Namespace, {6, NewAliases}).

namespace_locals(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 7).

get_local(Namespace, {Fun, Arity}) ->
  ordsets:filter(fun({F, A, _P}) -> (F == Fun) andalso (A == Arity) end,
                 namespace_locals(Namespace));
get_local(Namespace, Fun) ->
  ordsets:filter(fun({F, _A, _P}) -> F == Fun end,
                 namespace_locals(Namespace)).

add_local(Namespace, Fun, Arity, ParameterType) ->
  Locals = namespace_locals(Namespace),
  NewLocals = ordsets:add_element({Fun, Arity, ParameterType}, Locals),
  ets:update_element(kapok_namespaces, Namespace, {7, NewLocals}).

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
  build_namespace(?m(TEnv, namespace), TEnv, Callback).

handle_ast({list, Meta, [{identifier, _, ns}, {C1, _, _} = Ast, {C2, _, Doc} | Left]}, Env)
    when ?is_dot_id(C1), ?is_string(C2) ->
  handle_ns(Meta, Ast, Doc, Left, Env);

handle_ast({list, Meta, [{identifier, _, ns}, {C1, _, _} = Ast | Left]}, Env) when ?is_dot_id(C1) ->
  handle_ns(Meta, Ast, Left, Env);
handle_ast({list, Meta, [{identifier, _, Id}, {identifier, _, Name} | Left]}, Env) when ?is_def(Id) ->
  handle_def(Meta, Id, Name, Left, Env);

handle_ast({_, Meta, _} = Ast, Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {invalid_expression, {Ast}}).

%% namespace

handle_ns(Meta, Ast, Clauses, Env) ->
  handle_ns(Meta, Ast, <<"">>, Clauses,  Env).
handle_ns(Meta, {C, _, Arg} = Ast, _Doc, Clauses, Env) ->
  Name = case C of
           'dot' -> kapok_parser:dot_fullname(Ast);
           'identifier' -> Arg
         end,
  Env1 = case ?m(Env, namespace) of
           nil -> Env#{namespace => Name};
           NS -> kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {multiple_namespace, {NS, Name}})
         end,
  case ets:info(kapok_namespaces) of
    undefined -> init_namespace_table();
    _ -> ok
  end,
  add_namespace(Name),
  {_, TEnv1} = lists:mapfoldl(fun handle_namespace_clause/2, Env1, Clauses),
  TEnv1.

handle_namespace_clause({list, _, [{identifier, _, 'require'} | T]}, Env) ->
  handle_require_clause(T, Env);
handle_namespace_clause({list, _, [{identifier, _, 'use'} | T]}, Env) ->
  handle_use_clause(T, Env).

%% require
handle_require_clause(List, Env) when is_list(List) ->
  lists:mapfoldl(fun handle_require_element/2, Env, List).

handle_require_element({Category, Meta, Id}, Env) when ?is_local_id(Category) ->
  {Id, kapok_env:add_require(Meta, Env, Id)};
handle_require_element({dot, Meta, _} = Dot, Env) ->
  Name = kapok_parser:dot_fullname(Dot),
  {Name, kapok_env:add_require(Meta, Env, Name)};
handle_require_element({Category, Meta, Args}, Env) when ?is_list(Category) ->
  case Args of
    [{Category, _, _} = Ast, {keyword, _, 'as'}, {identifier, _, Id}] when ?is_local_id(Category) ->
      {Name, TEnv} = handle_require_element(Ast, Env),
      {Name, kapok_env:add_require(Meta, TEnv, Id, Name)};
    [{dot, _, _} = Ast, {keyword, _, 'as'}, {identifier, _, Id}] ->
      {Name, TEnv} = handle_require_element(Ast, Env),
      {Name, kapok_env:add_require(Meta, TEnv, Id, Name)};
    _ ->
      kapok_error:compile_error(Meta, ?m(Env, file), "invalid require expression ~p", [Args])
  end.

%% use
handle_use_clause(List, Env) when is_list(List) ->
  lists:mapfoldl(fun handle_use_element/2, Env, List).

handle_use_element({Category, Meta, Id}, Env) when ?is_local_id(Category) ->
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
    [{C, _, _} = Ast | T] when ?is_local_id(C) ->
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
handle_use_element_arguments(Meta, Name, Meta1, [{{keyword, _, 'as'}, {identifier, _, Id}} | T], Env) ->
  handle_use_element_arguments(Meta, Name, Meta1, T, kapok_env:add_require(Meta, Env, Id, Name));
handle_use_element_arguments(Meta, Name, _, [{{keyword, Meta1, 'exclude'}, {_, Meta2, Args}} | T], Env) ->
  Functions = parse_functions(Meta2, Args, Env),
  Env1 = kapok_env:add_use(Meta1, Env, Name, 'exclude', Functions),
  handle_use_element_arguments(Meta, Name, Meta1, T, Env1);
handle_use_element_arguments(Meta, Name, nil, [{{keyword, Meta1, 'only'}, {_, Meta2, Args}} | T], Env) ->
  Functions = parse_functions(Meta2, Args, Env),
  Env1 = kapok_env:add_use(Meta1, Env, Name, 'only', Functions),
  handle_use_element_arguments(Meta, Name, nil, T, Env1);
handle_use_element_arguments(_Meta, _Name, Meta1, [{{keyword, Meta2, 'only'}, {_, _, _}} | _T], Env) ->
  kapok_error:compile_error(Meta2, ?m(Env, file), "invalid usage of :only with :exclude present at line: ~p", [?line(Meta1)]);
handle_use_element_arguments(Meta, Name, Meta1, [{{keyword, _, 'rename'}, {_, _, Args}} | T], Env) ->
  Aliases = parse_function_aliases(Meta, Args, Env),
  NewEnv = kapok_env:add_use(Meta, Env, Name, 'rename', Aliases),
  handle_use_element_arguments(Meta, Name, Meta1, T, NewEnv);
handle_use_element_arguments(_Meta, _Name, _, [{_, Meta1, _} = Ast | _T], Env) ->
  kapok_error:compile_error(Meta1, ?m(Env, file), "invalid use argument ~p", [Ast]).


%% definitions
handle_def(Meta, Kind, Name, Ast, Env) when ?is_def_alias(Kind) ->
  handle_def_alias(Meta, Kind, Name, Ast, Env);
handle_def(Meta, Kind, Name, [{literal_list, _, _} = Args | T], Env) ->
  handle_def_with_args(Meta, Kind, Name, Args, T, Env);
handle_def(Meta, Kind, Name, [{C, _, _} = Doc | T], Env) when ?is_string(C) ->
  handle_def_with_doc(Meta, Kind, Name, Doc, T, Env);
handle_def(Meta, Kind, Name, Exprs, Env) ->
  handle_def_with_doc(Meta, Kind, Name, empty_def_doc(), Exprs, Env).

handle_def_alias(Meta, Kind, Alias, [{list, _, [{identifier, _, Fun}, {number, _, Arity}]}, {C, _, _} = _Doc], Env)
    when ?is_string(C) ->
  %% TODO add doc
  do_handle_def_alias(Meta, Kind, Alias, {Fun, Arity}, Env);
handle_def_alias(Meta, Kind, Alias, [{list, _, [{identifier, _, Fun}, {number, _, Arity}]}], Env) ->
  do_handle_def_alias(Meta, Kind, Alias, {Fun, Arity}, Env);
handle_def_alias(Meta, Kind, Alias, [{identifier, _, Fun}, {C, _, _} = _Doc], Env) when ?is_string(C) ->
  %% TODO add doc
  do_handle_def_alias(Meta, Kind, Alias, Fun, Env);
handle_def_alias(Meta, Kind, Alias, [{identifier, _, Fun}], Env) ->
  do_handle_def_alias(Meta, Kind, Alias, Fun, Env);
handle_def_alias(Meta, _Kind, Alias, Ast, Env) ->
  Error = {invalid_defalias_expression, {Alias, Ast}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error).

do_handle_def_alias(Meta, Kind, Alias, Original, Env) ->
  Namespace = ?m(Env, namespace),
  case get_local(Namespace, Original) of
    [] ->
      Error = {nonexistent_original_for_alias, {Alias, Original}},
      kapok_error:compile_error(Meta, ?m(Env, file), ?MODULE, Error);
    L ->
      add_alias(Namespace, Alias, Original),
      lists:map(fun({_F, A, P}) ->
                    add_local(Namespace, Alias, A, P),
                    add_export(Namespace, Kind, Alias, A, P)
                end,
                L)
  end,
  Env.

handle_def_with_args(Meta, Kind, Name, Args, [{list, _, [{identifier, _, 'when'} | _]} = Guard | Left], Env) ->
  handle_def_with_args(Meta, Kind, Name, Args, Guard, Left, Env);
handle_def_with_args(Meta, Kind, Name, Args, Left, Env) ->
  handle_def_with_args(Meta, Kind, Name, Args, [], Left, Env).
handle_def_with_args(Meta, Kind, Name, Args, Guard, [{C, _, _} = _Doc | Body], Env) when ?is_string(C) ->
  handle_def_clause(Meta, Kind, Name, Args, Guard, Body, Env);
handle_def_with_args(Meta, Kind, Name, Args, Guard, Body, Env) ->
  handle_def_clause(Meta, Kind, Name, Args, Guard, Body, Env).

handle_def_with_doc(Meta, Kind, Name, _Doc, Exprs, Env) ->
  %% TODO add doc
  handle_def_exprs(Meta, Kind, Name, Exprs, Env).

empty_def_doc() ->
  {bitstring, [], <<"">>}.

handle_def_exprs(_Meta, Kind, Name, Exprs, Env) ->
  lists:foldl(fun (Expr, E) -> handle_def_expr(Kind, Name, Expr, E) end, Env, Exprs).

handle_def_expr(Kind, Name, {list, Meta, [{literal_list, _, _} = Args, {list, _, [{identifier, _, 'when'} | _]} = Guard | Body]}, Env) ->
  handle_def_clause(Meta, Kind, Name, Args, Guard, Body, Env);
handle_def_expr(Kind, Name, {list, Meta, [{literal_list, _, _} = Args | Body]}, Env) ->
  handle_def_clause(Meta, Kind, Name, Args, [], Body, Env);
handle_def_expr(Kind, _Name, {_, Meta, _} = Ast, Env) ->
  Error = {invalid_def_expression, {Ast, Kind}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error).

handle_def_clause(Meta, Kind, Name, Args, Guard, Body, #{function := Function} = Env) ->
  %% TODO add doc
  Namespace = ?m(Env, namespace),
  Name1 = case Kind of
            'defmacro' -> kapok_utils:macro_name(Name);
            _ -> Name
          end,
  {TF, TEnv} = kapok_translate:translate(Name1, Env),
  case parse_parameters(Args, TEnv) of
    [{normal, _, NormalArgs}] ->
      {TArgs, TEnv1} = kapok_translate:translate_args(NormalArgs, TEnv),
      ParameterType = 'normal',
      Arity = length(TArgs),
      PrepareBody = [],
      Env5 = TEnv1;
    [{normal, _, NormalArgs}, {keyword_optional, _, OptionalParameters}] ->
      {TNormalArgs, TEnv1} = kapok_translate:translate_args(NormalArgs, TEnv),
      {TOptionalParameters, TEnv2} = translate_parameter_with_default(OptionalParameters, TEnv1),
      ParameterType = 'normal',
      TArgs = TNormalArgs ++ get_optional_args(TOptionalParameters),
      Arity = length(TArgs),
      PrepareBody = [],
      add_optional_clauses(Meta, Kind, Namespace, Name1, TF, TNormalArgs, TOptionalParameters, TEnv2),
      Env5 = TEnv2;
    [{normal, _, NormalArgs}, {keyword_rest, _, RestArgs}] ->
      {TNormalArgs, TEnv1} = kapok_translate:translate_args(NormalArgs, TEnv),
      {TRestArgs, TEnv2} = kapok_translate:translate_args(RestArgs, TEnv1),
      ParameterType = 'rest',
      TArgs = TNormalArgs ++ TRestArgs,
      Arity = length(TArgs),
      PrepareBody = [],
      add_rest_clause(Meta, Kind, Namespace, Name1, Arity - 1, TF, TNormalArgs, TEnv2),
      Env5 = TEnv2;
    [{normal, _, NormalArgs}, {keyword_key, Meta1, KeyParameters}] ->
      {TNormalArgs, TEnv1} = kapok_translate:translate_args(NormalArgs, TEnv),
      {TKeyParameters, TEnv2} = translate_parameter_with_default(KeyParameters, TEnv1),
      ParameterType = 'key',
      {TMapArg, TEnv3} = kapok_translate:translate_arg({identifier, Meta1, kapok_utils:gensym_with("M")}, TEnv2),
      TArgs = TNormalArgs ++ [TMapArg],
      Arity = length(TArgs),
      %% retrieve and map all key values from map argument to variables
      {PrepareBody, TEnv4} = kapok_translate:map_vars(Meta, TMapArg, TKeyParameters, TEnv3),
      add_key_clause(Meta, Kind, Namespace, Name1, Arity - 1, TF, TNormalArgs, TEnv4),
      Env5 = TEnv4;
    [{normal, _, NormalArgs}, {keyword_optional, _, OptionalParameters}, {keyword_rest, RestArgs}] ->
      {TNormalArgs, TEnv1} = kapok_translate:translate_args(NormalArgs, TEnv),
      {TOptionalParameters, TEnv2} = translate_parameter_with_default(OptionalParameters, TEnv1),
      {TRestArgs, TEnv3} = kapok_translate:translate_args(RestArgs, TEnv2),
      ParameterType = 'rest',
      TNonRestArgs = TNormalArgs ++ get_optional_args(TOptionalParameters),
      TArgs = TNonRestArgs ++ TRestArgs,
      Arity = length(TArgs),
      PrepareBody = [],
      add_rest_clause(Meta, Kind, Namespace, Name1, Arity - 1, TF, TNonRestArgs, TEnv3),
      add_optional_clauses(Meta, Kind, Namespace, Name1, TF, TNormalArgs, TOptionalParameters, TEnv3),
      Env5 = TEnv3
  end,
  Env6 = kapok_env:push_scope(Env5#{function => {Name1, Arity, ParameterType}}),
  {TGuard, TEnv6} = kapok_translate:translate_guard(Guard, Env6),
  {TBody, TEnv7} = kapok_translate:translate_body(Meta, Body, TEnv6),
  Clause = {clause, ?line(Meta), TArgs, TGuard, PrepareBody ++ TBody},
  add_clause(Namespace, Kind, Name1, Arity, Clause),
  add_export(Namespace, Kind, Name1, Arity, ParameterType),
  add_local(Namespace, Name1, Arity, ParameterType),
  kapok_env:pop_scope(TEnv7#{function => Function}).

%% parse parameters
parse_parameters({Category, _, Args}, Env) when Category == literal_list ->
  parse_parameters(Args, Env);
parse_parameters(Args, Env) when is_list(Args) ->
  L = parse_parameters(Args, [], {normal, [], []}, Env),
  lists:reverse(L).

parse_parameters([], Acc, {Previous, Meta, Args}, _Env) ->
  [{Previous, Meta, lists:reverse(Args)} | Acc];

parse_parameters([{Category, Meta} = Token], _Acc, {_Previous, _Meta, _Args}, Env)
    when ?is_parameter_keyword(Category) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {dangling_parameter_keyword, {Token}});

parse_parameters([{keyword_optional, Meta} | T], Acc, {normal, Meta1, Args}, Env) ->
  parse_parameters(T, [{normal, Meta1, lists:reverse(Args)} | Acc], {keyword_optional, Meta, []}, Env);
parse_parameters([{keyword_optional, Meta} = Token | _T], _Acc, {Previous, Meta1, _Args}, Env) ->
  Error = {invalid_postion_of_parameter_keyword, {Token, {Previous, Meta1}}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);

parse_parameters([{keyword_rest, Meta} | T], Acc, {normal, Meta1, Args}, Env) ->
  parse_parameters(T, [{normal, Meta1, lists:reverse(Args)} | Acc], {keyword_rest, Meta, []}, Env);
parse_parameters([{keyword_rest, Meta} | T], Acc, {keyword_optional, Meta1, Args}, Env) ->
  Last = {keyword_optional, Meta1, lists:reverse(Args)},
  parse_parameters(T, [Last | Acc], {keyword_rest, Meta, []}, Env);
parse_parameters([{keyword_rest, Meta} = Token | _T], _Acc, {Previous, Meta1, _Args}, Env) ->
  Error = {invalid_postion_of_parameter_keyword, {Token, {Previous, Meta1}}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);

parse_parameters([{keyword_key, Meta} | T], Acc, {normal, Meta1, Args}, Env) ->
  parse_parameters(T, [{normal, Meta1, lists:reverse(Args)} | Acc], {keyword_key, Meta, []}, Env);
parse_parameters([{keyword_key, Meta} | _T], _Acc, {Previous, Meta1, _Args}, Env) ->
  Error = {invalid_postion_of_parameter_keyword, {keyword_key, {Previous, Meta1}}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);

parse_parameters([H | T], Acc, {normal, Meta, Args}, Env) ->
  parse_parameters(T, Acc, {normal, Meta, [H | Args]}, Env);

parse_parameters([{list, _Meta, [Expr, Default]} | T], Acc, {keyword_optional, Meta1, Args}, Env) ->
  parse_parameters(T, Acc, {keyword_optional, Meta1, [{Expr, Default} | Args]}, Env);
parse_parameters([{identifier, Meta, _} = Id | T], Acc, {keyword_optional, Meta1, Args}, Env) ->
  parse_parameters(T, Acc, {keyword_optional, Meta1, [{Id, {atom, Meta, 'nil'}} | Args]}, Env);
parse_parameters([{_, Meta, _} = H | _T], _Acc, {keyword_optional, Meta1, _Args}, Env) ->
  Error = {invalid_parameter, {H, {keyword_optional, Meta1}}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);

parse_parameters([{_, Meta, _} = H | _T], _Acc, {keyword_rest, Meta1, Args}, Env) when Args /= [] ->
  Error = {too_many_parameters_for_keyword, {H, {keyword_rest, Meta1}}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);

parse_parameters([{list, _Meta, [{identifier, _, _} = Expr, Default]} | T], Acc, {keyword_key, Meta1, Args}, Env) ->
  parse_parameters(T, Acc, {keyword_key, Meta1, [{Expr, Default} | Args]}, Env);
parse_parameters([{identifier, Meta, _} = Id | T], Acc, {keyword_key, Meta1, Args}, Env) ->
  parse_parameters(T, Acc, {keyword_key, Meta1, [{Id, {atom, Meta, 'nil'}} | Args]}, Env);
parse_parameters([{_, Meta, _} = H | _T], _Acc, {keyword_key, Meta1, _Args}, Env) ->
  Error = {invalid_parameter, {H, {keyword_key, Meta1}}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);

parse_parameters([H | T], Acc, {Previous, Meta1, Args}, Env) ->
  parse_parameters(T, Acc, {Previous, Meta1, [H | Args]}, Env).

get_optional_args(OptionalParameters) ->
  lists:map(fun({Expr, _Default}) -> Expr end, OptionalParameters).

translate_parameter_with_default(Parameters, Env) ->
  {L, TEnv} = lists:foldl(fun({Expr, Default}, {Acc, E}) ->
                              {TExpr, E1} = kapok_translate:translate_arg(Expr, E),
                              {TDefault, E2} = kapok_translate:translate_arg(Default, E1),
                              {[{TExpr, TDefault} | Acc], E2}
                          end,
                          {[], Env},
                          Parameters),
  {lists:reverse(L), TEnv}.

add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs, TOptionalParameters, Env) ->
  R = lists:reverse(TOptionalParameters),
  do_add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs, R, Env).
do_add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs, TReversedOptionalParameters, Env) ->
  case TReversedOptionalParameters of
    [{_Expr, Default} | Left] ->
      TArgs = TNormalArgs ++ get_optional_args(lists:reverse(Left)),
      Arity = length(TArgs),
      add_clause_with_extra_arg(Meta, Kind, Namespace, Name, Arity, TF, TArgs, Default),
      do_add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs, Left, Env);
    [] ->
      ok
  end.

add_rest_clause(Meta, Kind, Namespace, Name, Arity, TF, TNormalArgs, Env) ->
  {TListArgs, _} = kapok_translate:translate({literal_list, [], []}, Env),
  add_clause_with_extra_arg(Meta, Kind, Namespace, Name, Arity, TF, TNormalArgs, TListArgs).

add_key_clause(Meta, Kind, Namespace, Name, Arity, TF, TNormalArgs, Env) ->
  {TMapArg, _} = kapok_translate:translate({map, [], []}, Env),
  add_clause_with_extra_arg(Meta, Kind, Namespace, Name, Arity, TF, TNormalArgs, TMapArg).

add_clause_with_extra_arg(Meta, Kind, Namespace, Name, Arity, TF, TNormalArgs, Extra) ->
  %% redirect normal call(without &rest/&key argument) to the clause with extra argument
  TArgs = TNormalArgs ++ [Extra],
  TBody = [{call, ?line(Meta), TF, TArgs}],
  add_clause(Namespace, Kind, Name, Arity, {clause, ?line(Meta), TNormalArgs, [], TBody}),
  add_export(Namespace, Kind, Name, Arity, 'normal'),
  add_local(Namespace, Name, Arity, 'normal').


%% namespace
build_namespace(Namespace, Env, Callback) ->
  Functions = namespace_export_functions(Namespace),
  Macros = namespace_export_macros(Namespace),
  Defs = namespace_defs(Namespace),
  Aliases = namespace_aliases(Namespace, Defs),
  build_module(Namespace, Functions, Macros, Defs, Aliases, Env, Callback).

%% module

build_module(Module, Functions, Macros, Defs, Aliases, Env, Callback) ->
  Exports = module_exports(Functions, Macros),
  FunInfo = info_fun(Functions, Macros, Env),
  TranslateFun = fun({Fun, Arity}, Clauses, Acc) ->
                     [{function, 0, Fun, Arity, lists:reverse(Clauses)} | Acc]
                 end,
  FunDefs = orddict:fold(TranslateFun, [], Defs),
  FunAlieses = orddict:fold(TranslateFun, [], Aliases),
  Funs = FunDefs ++ FunAlieses,
  AttrModule = {attribute, 0, module, Module},
  AttrExport = {attribute, 0, export, ordsets:to_list(Exports)},
  AttrInfo = {attribute, 0, export, [{'__info__', 1}]},
  Erl = [AttrModule, AttrExport, AttrInfo, FunInfo | Funs],
  kapok_compiler:module(Erl, [], Env, Callback).

%% Helpers

group_arguments(Meta, Args, Env) ->
  group_arguments(Meta, Args, orddict:new(), Env).
group_arguments(_Meta, [], Acc, _Env) ->
  lists:map(fun({_, KV}) -> KV end, orddict:to_list(Acc));
group_arguments(Meta, [{keyword, _, 'as'} = K, {identifier, _, _} = V | T], Acc, Env) ->
  group_arguments(Meta, T, orddict:store('as', {K, V}, Acc), Env);
group_arguments(Meta, [{keyword, _, 'only'} = K, {C, _, _} = V | T], Acc, Env) when ?is_list(C) ->
  group_arguments(Meta, T, orddict:store('only', {K, V}, Acc), Env);
group_arguments(Meta, [{keyword, _, 'exclude'} = K, {C, _, _} = V | T], Acc, Env) when ?is_list(C) ->
  group_arguments(Meta, T, orddict:store('exclude', {K, V}, Acc), Env);
group_arguments(Meta, [{keyword, _, 'rename'} = K, {C, _, _} = V | T], Acc, Env) when ?is_list(C) ->
  group_arguments(Meta, T, orddict:store('rename', {K, V}, Acc), Env);
group_arguments(Meta, Args, _Acc, Env) ->
  kapok_error:compile_error(Meta, ?m(Env, file), "invalid use arguments: ~p~n", [Args]).

parse_functions(Meta, Args, Env) ->
  Fun = fun({list, _, [{identifier, _, Id}, {number, _, Integer}]}) when is_integer(Integer) -> {Id, Integer};
           ({Category, _, Id}) when ?is_local_id(Category) -> Id;
           (Other) -> kapok_error:compile_error(Meta, ?m(Env, file), "invalid function: ~p", [Other])
        end,
  ordsets:from_list(lists:map(Fun, Args)).


parse_function_aliases(Meta, Args, Env) ->
  Fun = fun({Category, _, [{list, _, [{identifier, _, Id}, {number, _, Integer}]}, {identifier, _, Alias}]})
              when ?is_list(Category), is_integer(Integer) ->
            {Alias, {Id, Integer}};
           ({Category, _, [{C1, _, Id}, {C2, _, Alias}]}) when ?is_list(Category), ?is_local_id(C1), ?is_local_id(C2) ->
            {Alias, Id};
           (Other) ->
            kapok_error:compile_error(Meta, ?m(Env, file), "invalid rename arguments: ~p", [Other])
        end,
  ordsets:from_list(lists:map(Fun, Args)).

%% Error
format_error({nonexistent_original_for_alias, {Alias, {Fun, Arity}}}) ->
  io_lib:format("fail to define aliases ~s because original function (~s ~B) does not exist", [Alias, Fun, Arity]);
format_error({nonexistent_original_for_alias, {Alias, Fun}}) ->
  io_lib:format("fail to define aliases ~s because original function ~s does not exist", [Alias, Fun]);
format_error({invalid_defalias_expression, {Alias, Ast}}) ->
  io_lib:format("invalid expression ~p to define alias ~s", [Ast, Alias]);
format_error({invalid_expression, {Ast}}) ->
  io_lib:format("invalid expression ~p", [Ast]);
format_error({multiple_namespace, {Existing, New}}) ->
  io_lib:format("multiple namespace in one file not supported: ~p, ~p", [Existing, New]);
format_error({dangling_parameter_keyword, {Token}}) ->
  io_lib:format("dangling ~s without argument", [token_text(Token)]);
format_error({invalid_def_expression, {Expr, Kind}}) ->
  io_lib:format("invalid expression ~p for ~s", [Expr, Kind]);
format_error({invalid_postion_of_parameter_keyword, {Token, Previous}}) ->
  io_lib:format("invalid ~s with ~s ahead at line ~B", [token_text(Token),
                                                        token_text(Previous),
                                                        ?line(token_meta(Previous))]);
format_error({invalid_parameter, {P, Token}}) ->
  io_lib:format("invalid parameter ~p for ~s at line ~B", [P, token_text(Token), ?line(token_meta(Token))]);
format_error({too_many_parameters_for_keyword, {P, Token}}) ->
  io_lib:format("too many parameters ~p for ~s", [P, token_text(Token)]).

