%% ast handlings, which include expanding/translating the kapok ast
-module(kapok_ast).
-export([compile/3,
         format_error/1]).
-import(kapok_scanner, [token_text/1, token_meta/1]).
-include("kapok.hrl").

compile(Ast, Env, Callback) when is_list(Ast) ->
  TEnv = lists:foldl(fun compile/2, Env, Ast),
  build_namespace(?m(TEnv, namespace), TEnv, Callback).

compile(Ast, Env) ->
  {EAst, EEnv} = kapok_macro:expand(Ast, Env),
  handle(EAst, EEnv).

handle({list, Meta, [{identifier, _, 'ns'} | T]}, Env) ->
  handle_ns(Meta, T, Env);
handle({list, Meta, [{identifier, _, Id} | T]}, Env) when ?is_def(Id) ->
  handle_def(Meta, Id, T, Env#{def_kind => Id});
handle({list, _Meta, [{identifier, _, Id} | _T]} = Ast, Env) when ?is_attr(Id) ->
  {TAttr, TEnv} = kapok_trans:translate(Ast, Env),
  Namespace = ?m(Env, namespace),
  kapok_symbol_table:add_form(Namespace, TAttr),
  TEnv;
handle(Ast, Env) ->
  kapok_error:form_error(token_meta(Ast), ?m(Env, file), ?MODULE, {invalid_expression, {Ast}}).

%% namespace

handle_ns(Meta, [{C1, _, _} = Ast, {C2, _, Doc} | T], Env) when ?is_dot_id(C1), ?is_string(C2) ->
  handle_ns(Meta, Ast, Doc, T, Env);
handle_ns(Meta, [{C1, _, _} = Ast | T], Env) when ?is_dot_id(C1) ->
  handle_ns(Meta, Ast, T, Env);
handle_ns(Meta, _T, Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {invalid_body, {'ns'}}).

handle_ns(Meta, Ast, Clauses, Env) ->
  handle_ns(Meta, Ast, <<"">>, Clauses,  Env).
handle_ns(Meta, {C, _, Arg} = Ast, _Doc, Clauses, Env) ->
  Name = case C of
           'dot' -> kapok_parser:dot_fullname(Ast);
           'identifier' -> Arg
         end,
  Env1 = case ?m(Env, namespace) of
           nil -> Env#{namespace => Name};
           NS -> kapok_error:form_error(Meta, ?m(Env, file), ?MODULE,
                                        {multiple_namespace, {NS, Name}})
         end,
  kapok_symbol_table:add_namespace(Name),
  {_, TEnv1} = lists:mapfoldl(fun handle_ns_clause/2, Env1, Clauses),
  Env2 = add_default_uses(TEnv1),
  Env2.

handle_ns_clause({list, _, [{identifier, _, 'require'} | T]}, Env) ->
  handle_require_clause(T, Env);
handle_ns_clause({list, _, [{identifier, _, 'use'} | T]}, Env) ->
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
handle_use_element_arguments(Meta, Name, Meta1, [{{keyword, _, 'as'}, {identifier, _, Id}} | T],
                             Env) ->
  handle_use_element_arguments(Meta, Name, Meta1, T, kapok_env:add_require(Meta, Env, Id, Name));
handle_use_element_arguments(Meta, Name, _, [{{keyword, Meta1, 'exclude'}, {_, Meta2, Args}} | T],
                             Env) ->
  Functions = parse_functions(Meta2, Args, Env),
  Env1 = kapok_env:add_use(Meta1, Env, Name, 'exclude', Functions),
  handle_use_element_arguments(Meta, Name, Meta1, T, Env1);
handle_use_element_arguments(Meta, Name, nil, [{{keyword, Meta1, 'only'}, {_, Meta2, Args}} | T],
                             Env) ->
  Functions = parse_functions(Meta2, Args, Env),
  Env1 = kapok_env:add_use(Meta1, Env, Name, 'only', Functions),
  handle_use_element_arguments(Meta, Name, nil, T, Env1);
handle_use_element_arguments(_Meta, _Name, Meta1, [{{keyword, Meta2, 'only'}, {_, _, _}} | _T],
                             Env) ->
  kapok_error:compile_error(Meta2,
                            ?m(Env, file),
                            "invalid usage of :only with :exclude present at line: ~p",
                            [?line(Meta1)]);
handle_use_element_arguments(Meta, Name, Meta1, [{{keyword, _, 'rename'}, {_, _, Args}} | T],
                             Env) ->
  Aliases = parse_function_aliases(Meta, Args, Env),
  NewEnv = kapok_env:add_use(Meta, Env, Name, 'rename', Aliases),
  handle_use_element_arguments(Meta, Name, Meta1, T, NewEnv);
handle_use_element_arguments(_Meta, _Name, _, [Ast | _T], Env) ->
  kapok_error:compile_error(token_meta(Ast), ?m(Env, file), "invalid use argument ~p", [Ast]).

%% Check whether the default namespaces is used. Add them if they are not declared in use clause.
add_default_uses(Env) ->
  lists:foldl(fun(Ns, #{uses := Uses} = E) ->
                  case orddict:is_key(Ns, Uses) of
                    true ->
                      E;
                    false ->
                      {_, E1} = handle_use_element({identifier, [], Ns}, E),
                      E1
                  end
              end,
              Env,
              kapok_dispatch:default_uses()).

%% definitions
handle_def(Meta, Kind, [{identifier, _, Name}  | T], Env) ->
  handle_def(Meta, Kind, Name, T, Env);
handle_def(Meta, Kind, _T, Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {invalid_body, {Kind}}).

handle_def(Meta, Kind, Name, T, Env) when ?is_def_alias(Kind) ->
  handle_def_alias(Meta, Kind, Name, T, Env);
handle_def(Meta, Kind, _Name, [], Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {invalid_body, {Kind}});
handle_def(Meta, Kind, Name, [{literal_list, _, _} = Args | T], Env) ->
  handle_def_with_args(Meta, Kind, Name, Args, T, Env);
handle_def(Meta, Kind, Name, [{C, _, _} = Doc | T], Env) when ?is_string(C) ->
  handle_def_with_doc(Meta, Kind, Name, Doc, T, Env);
handle_def(Meta, Kind, Name, Exprs, Env) ->
  handle_def_with_doc(Meta, Kind, Name, empty_def_doc(), Exprs, Env).

handle_def_alias(Meta, _Kind, Alias,
                 [{list, _, [{identifier, _, Fun}, {number, _, Arity}]}, {C, _, _} = _Doc], Env)
    when ?is_string(C) ->
  %% TODO add doc
  do_handle_def_alias(Meta, Alias, {Fun, Arity}, Env);
handle_def_alias(Meta, _Kind, Alias, [{list, _, [{identifier, _, Fun}, {number, _, Arity}]}], Env) ->
  do_handle_def_alias(Meta, Alias, {Fun, Arity}, Env);
handle_def_alias(Meta, _Kind, Alias, [{identifier, _, Fun}, {C, _, _} = _Doc], Env)
    when ?is_string(C) ->
  %% TODO add doc
  do_handle_def_alias(Meta, Alias, Fun, Env);
handle_def_alias(Meta, _Kind, Alias, [{identifier, _, Fun}], Env) ->
  do_handle_def_alias(Meta, Alias, Fun, Env);
handle_def_alias(Meta, _Kind, Alias, Ast, Env) ->
  Error = {invalid_defalias_expression, {Alias, Ast}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error).

do_handle_def_alias(Meta, Alias, Original, Env) ->
  Namespace = ?m(Env, namespace),
  case kapok_symbol_table:add_alias(Namespace, Alias, Original) of
    not_exist ->
      Error = {nonexistent_original_for_alias, {Alias, Original}},
      kapok_error:compile_error(Meta, ?m(Env, file), ?MODULE, Error);
    ok ->
      ok
  end,
  Env.

handle_def_with_args(Meta, Kind, Name, Args, [{list, _, [{identifier, _, 'when'} | _]} = Guard | T],
                     Env) ->
  handle_def_with_args(Meta, Kind, Name, Args, Guard, T, Env);
handle_def_with_args(Meta, Kind, Name, Args, T, Env) ->
  handle_def_with_args(Meta, Kind, Name, Args, [], T, Env).
handle_def_with_args(Meta, Kind, Name, Args, Guard, [{C, _, _} = _Doc | Body], Env)
    when ?is_string(C) ->
  %% TODO add doc
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

handle_def_expr(Kind,
                Name,
                {list, Meta, [{literal_list, _, _} = Args,
                              {list, _, [{identifier, _, 'when'} | _]} = Guard | Body]},
                Env) ->
  handle_def_clause(Meta, Kind, Name, Args, Guard, Body, Env);
handle_def_expr(Kind, Name, {list, Meta, [{literal_list, _, _} = Args | Body]}, Env) ->
  handle_def_clause(Meta, Kind, Name, Args, [], Body, Env);
handle_def_expr(Kind, _Name, Ast, Env) ->
  Error = {invalid_def_expression, {Ast, Kind}},
  kapok_error:form_error(token_meta(Ast), ?m(Env, file), ?MODULE, Error).

handle_def_clause(Meta, Kind, Name, Args, Guard, Body, #{def_fap := FAP} = Env) ->
  %% TODO add doc
  Namespace = ?m(Env, namespace),
  {TF, TEnv} = kapok_trans:translate(Name, kapok_env:push_scope(Env)),
  case parse_parameters(Args, TEnv) of
    [{normal, _, NormalArgs}] ->
      {TArgs, CEnv} = kapok_trans:translate_def_args(NormalArgs, TEnv),
      ParameterType = 'normal',
      PrepareBody = [];
    [{normal, _, NormalArgs}, {keyword_optional, _, OptionalParameters}] ->
      {TNormalArgs, TEnv1} = kapok_trans:translate_def_args(NormalArgs, TEnv),
      {TOptionalParameters, CEnv} = translate_parameter_with_default(OptionalParameters, TEnv1),
      ParameterType = 'normal',
      TArgs = TNormalArgs ++ get_optional_args(TOptionalParameters),
      PrepareBody = [],
      add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs, TOptionalParameters, CEnv);
    [{normal, _, NormalArgs}, {keyword_rest, _, RestArgs}] ->
      {TNormalArgs, TEnv1} = kapok_trans:translate_def_args(NormalArgs, TEnv),
      {TRestArgs, CEnv} = kapok_trans:translate_def_args(RestArgs, TEnv1),
      ParameterType = 'rest',
      TArgs = TNormalArgs ++ TRestArgs,
      PrepareBody = [],
      add_rest_clause(Meta, Kind, Namespace, Name, TF, TNormalArgs, CEnv);
    [{normal, _, NormalArgs}, {keyword_key, Meta1, KeyParameters}] ->
      {TNormalArgs, TEnv1} = kapok_trans:translate_def_args(NormalArgs, TEnv),
      {TKeyParameters, TEnv2} = translate_parameter_with_default(KeyParameters, TEnv1),
      ParameterType = 'key',
      {TMapArg, TEnv3} = kapok_trans:translate_def_arg({identifier,
                                                        Meta1,
                                                        kapok_utils:gensym_with("KV")},
                                                       TEnv2),
      TArgs = TNormalArgs ++ [TMapArg],

      %% retrieve and map all key values from map argument to variables
      {PrepareBody, CEnv} = kapok_trans:map_vars(Meta, TMapArg, TKeyParameters, TEnv3),
      add_key_clause(Meta, Kind, Namespace, Name, TF, TNormalArgs, CEnv);
    [{normal, _, NormalArgs}, {keyword_optional, _, OptionalParameters},
     {keyword_rest, RestArgs}] ->
      {TNormalArgs, TEnv1} = kapok_trans:translate_def_args(NormalArgs, TEnv),
      {TOptionalParameters, TEnv2} = translate_parameter_with_default(OptionalParameters, TEnv1),
      {TRestArgs, CEnv} = kapok_trans:translate_def_args(RestArgs, TEnv2),
      ParameterType = 'rest',
      TNonRestArgs = TNormalArgs ++ get_optional_args(TOptionalParameters),
      TArgs = TNonRestArgs ++ TRestArgs,
      PrepareBody = [],
      add_rest_clause(Meta, Kind, Namespace, Name, TF, TNonRestArgs, CEnv),
      add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs, TOptionalParameters, CEnv)
  end,
  Arity = length(TArgs),
  Env6 = CEnv#{def_fap => {Name, Arity, ParameterType}},
  %% TODO add def_fap for other clauses(maybe do this when impl delay compilation stuff?)
  {TGuard, TEnv7} = kapok_trans:translate_guard(Guard, Env6),
  {TBody, TEnv8} = kapok_trans:translate_body(Meta, Body, TEnv7),
  Clause = {clause, ?line(Meta), TArgs, TGuard, PrepareBody ++ TBody},
  kapok_symbol_table:add_def(Namespace, Kind, Name, Arity, ParameterType, Clause),
  kapok_env:pop_scope(TEnv8#{def_fap => FAP}).

%% parse parameters
parse_parameters({Category, _, Args}, Env) when Category == literal_list ->
  parse_parameters(Args, Env);
parse_parameters(Args, Env) when is_list(Args) ->
  L = parse_parameters(Args, [], {normal, [], []}, Env),
  lists:reverse(L).

parse_parameters([], Acc, {Previous, Meta, Args}, _Env) ->
  [{Previous, Meta, lists:reverse(Args)} | Acc];

parse_parameters([{Category, Meta, _} = Token], _Acc, {_Previous, _Meta, _Args}, Env)
    when ?is_parameter_keyword(Category) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {dangling_parameter_keyword, {Token}});

parse_parameters([{keyword_optional, Meta, _} | T], Acc, {normal, Meta1, Args}, Env) ->
  parse_parameters(T, [{normal, Meta1, lists:reverse(Args)} | Acc], {keyword_optional, Meta, []},
                   Env);
parse_parameters([{keyword_optional, Meta, _} = Token | _T], _Acc, {Previous, Meta1, _Args}, Env) ->
  Error = {invalid_postion_of_parameter_keyword, {Token, {Previous, Meta1}}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);

parse_parameters([{keyword_rest, Meta, _} | T], Acc, {normal, Meta1, Args}, Env) ->
  parse_parameters(T, [{normal, Meta1, lists:reverse(Args)} | Acc], {keyword_rest, Meta, []}, Env);
parse_parameters([{keyword_rest, Meta, _} | T], Acc, {keyword_optional, Meta1, Args}, Env) ->
  Last = {keyword_optional, Meta1, lists:reverse(Args)},
  parse_parameters(T, [Last | Acc], {keyword_rest, Meta, []}, Env);
parse_parameters([{keyword_rest, Meta, _} = Token | _T], _Acc, {Previous, Meta1, _Args}, Env) ->
  Error = {invalid_postion_of_parameter_keyword, {Token, {Previous, Meta1}}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);

parse_parameters([{keyword_key, Meta, _} | T], Acc, {normal, Meta1, Args}, Env) ->
  parse_parameters(T, [{normal, Meta1, lists:reverse(Args)} | Acc], {keyword_key, Meta, []}, Env);
parse_parameters([{keyword_key, Meta, _} | _T], _Acc, {Previous, Meta1, _Args}, Env) ->
  Error = {invalid_postion_of_parameter_keyword, {keyword_key, {Previous, Meta1}}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);

parse_parameters([H | T], Acc, {normal, Meta, Args}, Env) ->
  parse_parameters(T, Acc, {normal, Meta, [H | Args]}, Env);

parse_parameters([{list, _Meta, [Expr, Default]} | T], Acc, {keyword_optional, Meta1, Args}, Env) ->
  parse_parameters(T, Acc, {keyword_optional, Meta1, [{Expr, Default} | Args]}, Env);
parse_parameters([{identifier, Meta, _} = Id | T], Acc, {keyword_optional, Meta1, Args}, Env) ->
  parse_parameters(T, Acc, {keyword_optional, Meta1, [{Id, {atom, Meta, 'nil'}} | Args]}, Env);
parse_parameters([H | _T], _Acc, {keyword_optional, Meta1, _Args}, Env) ->
  Error = {invalid_parameter, {H, {keyword_optional, Meta1}}},
  kapok_error:form_error(token_meta(H), ?m(Env, file), ?MODULE, Error);

parse_parameters([H | _T], _Acc, {keyword_rest, Meta1, Args}, Env) when Args /= [] ->
  Error = {too_many_parameters_for_keyword, {H, {keyword_rest, Meta1}}},
  kapok_error:form_error(token_meta(H), ?m(Env, file), ?MODULE, Error);

parse_parameters([{list, _Meta, [{identifier, _, _} = Expr, Default]} | T],
                 Acc,
                 {keyword_key, Meta1, Args},
                 Env) ->
  parse_parameters(T, Acc, {keyword_key, Meta1, [{Expr, Default} | Args]}, Env);
parse_parameters([{identifier, Meta, _} = Id | T], Acc, {keyword_key, Meta1, Args}, Env) ->
  parse_parameters(T, Acc, {keyword_key, Meta1, [{Id, {atom, Meta, 'nil'}} | Args]}, Env);
parse_parameters([H | _T], _Acc, {keyword_key, Meta1, _Args}, Env) ->
  Error = {invalid_parameter, {H, {keyword_key, Meta1}}},
  kapok_error:form_error(token_meta(H), ?m(Env, file), ?MODULE, Error);

parse_parameters([H | T], Acc, {Previous, Meta1, Args}, Env) ->
  parse_parameters(T, Acc, {Previous, Meta1, [H | Args]}, Env).

get_optional_args(OptionalParameters) ->
  lists:map(fun({Expr, _Default}) -> Expr end, OptionalParameters).

translate_parameter_with_default(Parameters, Env) ->
  {L, TEnv} = lists:foldl(fun({Expr, Default}, {Acc, E}) ->
                              {TExpr, E1} = kapok_trans:translate_def_arg(Expr, E),
                              {TDefault, E2} = kapok_trans:translate_def_arg(Default, E1),
                              {[{TExpr, TDefault} | Acc], E2}
                          end,
                          {[], Env},
                          Parameters),
  {lists:reverse(L), TEnv}.

add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs, TOptionalParameters, Env) ->
  R = lists:reverse(TOptionalParameters),
  do_add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs, R, Env).
do_add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs, TReversedOptionalParameters,
                        Env) ->
  case TReversedOptionalParameters of
    [{_Expr, Default} | Left] ->
      TArgs = TNormalArgs ++ get_optional_args(lists:reverse(Left)),
      add_redirect_clause(Meta, Kind, Namespace, Name, TF, TArgs, Default),
      do_add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs, Left, Env);
    [] ->
      ok
  end.

add_rest_clause(Meta, Kind, Namespace, Name, TF, TNormalArgs, Env) ->
  {TListArgs, _} = kapok_trans:translate({literal_list, [], []}, Env),
  add_redirect_clause(Meta, Kind, Namespace, Name, TF, TNormalArgs, TListArgs).

add_key_clause(Meta, Kind, Namespace, Name, TF, TNormalArgs, Env) ->
  {TMapArg, _} = kapok_trans:translate({map, [], []}, Env),
  add_redirect_clause(Meta, Kind, Namespace, Name, TF, TNormalArgs, TMapArg).

add_redirect_clause(Meta, Kind, Namespace, Name, TF, TNormalArgs, Extra) ->
  %% redirect normal call(without &rest/&key argument) to the clause with an extra default argument
  Arity = length(TNormalArgs),
  TArgs = TNormalArgs ++ [Extra],
  TBody = [{call, ?line(Meta), TF, TArgs}],
  kapok_symbol_table:add_def(Namespace, Kind, Name, Arity, 'normal',
                             {clause, ?line(Meta), TNormalArgs, [], TBody}).

%% namespace
build_namespace(Namespace, Env, Callback) ->
  {Forms, Env1} = kapok_symbol_table:namespace_forms(Namespace, Namespace, Env),
  kapok_erl:module(Forms, [], Env1, Callback).

%% Helpers

group_arguments(Meta, Args, Env) ->
  group_arguments(Meta, Args, orddict:new(), Env).
group_arguments(_Meta, [], Acc, _Env) ->
  lists:map(fun({_, KV}) -> KV end, orddict:to_list(Acc));
group_arguments(Meta, [{keyword, _, 'as'} = K, {identifier, _, _} = V | T], Acc, Env) ->
  group_arguments(Meta, T, orddict:store('as', {K, V}, Acc), Env);
group_arguments(Meta, [{keyword, _, 'only'} = K, {C, _, _} = V | T], Acc, Env)
    when ?is_list(C) ->
  group_arguments(Meta, T, orddict:store('only', {K, V}, Acc), Env);
group_arguments(Meta, [{keyword, _, 'exclude'} = K, {C, _, _} = V | T], Acc, Env)
    when ?is_list(C) ->
  group_arguments(Meta, T, orddict:store('exclude', {K, V}, Acc), Env);
group_arguments(Meta, [{keyword, _, 'rename'} = K, {C, _, _} = V | T], Acc, Env)
    when ?is_list(C) ->
  group_arguments(Meta, T, orddict:store('rename', {K, V}, Acc), Env);
group_arguments(Meta, Args, _Acc, Env) ->
  kapok_error:compile_error(Meta, ?m(Env, file), "invalid use arguments: ~p~n", [Args]).

parse_functions(Meta, Args, Env) ->
  Fun = fun({list, _, [{identifier, _, Id}, {number, _, Integer}]}) when is_integer(Integer) ->
            {Id, Integer};
           ({Category, _, Id}) when ?is_local_id(Category) ->
            Id;
           (Other) ->
            kapok_error:compile_error(Meta, ?m(Env, file), "invalid function: ~p", [Other])
        end,
  ordsets:from_list(lists:map(Fun, Args)).


parse_function_aliases(Meta, Args, Env) ->
  Fun = fun({Category, _, [{list, _, [{identifier, _, Id},
                                      {number, _, Integer}]},
                           {identifier, _, Alias}]})
              when ?is_list(Category), is_integer(Integer) ->
            {Alias, {Id, Integer}};
           ({Category, _, [{C1, _, Id}, {C2, _, Alias}]})
              when ?is_list(Category), ?is_local_id(C1), ?is_local_id(C2) ->
            {Alias, Id};
           (Other) ->
            kapok_error:compile_error(Meta, ?m(Env, file), "invalid rename arguments: ~p", [Other])
        end,
  ordsets:from_list(lists:map(Fun, Args)).

%% Error
format_error({invalid_expression, {Ast}}) ->
  io_lib:format("invalid top level expression ~s", [token_text(Ast)]);
format_error({invalid_body, {Form}}) ->
  io_lib:format("invalid body for form: ~p", [Form]);
format_error({nonexistent_original_for_alias, {Alias, {Fun, Arity}}}) ->
  io_lib:format("fail to define aliases ~s because original function (~s ~B) does not exist",
                [Alias, Fun, Arity]);
format_error({nonexistent_original_for_alias, {Alias, Fun}}) ->
  io_lib:format("fail to define aliases ~s because original function ~s does not exist",
                [Alias, Fun]);
format_error({invalid_defalias_expression, {Alias, Ast}}) ->
  io_lib:format("invalid expression ~p to define alias ~s", [Ast, Alias]);
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
  io_lib:format("invalid parameter ~p for ~s at line ~B",
                [P, token_text(Token), ?line(token_meta(Token))]);
format_error({too_many_parameters_for_keyword, {P, Token}}) ->
  io_lib:format("too many parameters ~p for ~s", [P, token_text(Token)]).
