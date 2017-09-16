%% ast handlings, which include expanding/translating the kapok ast
-module(kapok_ast).
-export([compile/2,
         format_error/1,
         empty_doc/0]).
-import(kapok_scanner, [token_text/1, token_meta/1]).
-import(kapok_env, [get_compiler_opt/1]).
-include("kapok.hrl").

compile(Ast, Ctx) when is_list(Ast) ->
  Ctx1 = lists:foldl(fun compile/2, Ctx, Ast),
  build_namespace(?m(Ctx1, namespace), Ctx1);
compile(Ast, Ctx) ->
  {EAst, ECtx} = kapok_macro:expand(Ast, Ctx),
  handle(EAst, ECtx).

handle({list, Meta, [{identifier, _, 'ns'} | T]}, Ctx) ->
  handle_ns(Meta, T, Ctx);
handle({list, Meta, [{identifier, _, Id} | T]}, Ctx) when ?is_def_ns(Id) ->
  handle_def_ns(Meta, Id, T, Ctx);
handle({list, Meta, [{identifier, _, Id} | T]} = Ast, Ctx) when ?is_def(Id) ->
  handle_def(Meta, Id, T, Ctx#{def_kind => Id, def_ast => Ast});
handle({list, _Meta, [{identifier, _, Id} | _T]} = Ast, Ctx) when ?is_attr(Id) ->
  {TAttr, TCtx} = kapok_trans:translate(Ast, Ctx),
  Namespace = ?m(Ctx, namespace),
  kapok_symbol_table:add_form(Namespace, TAttr),
  TCtx;
handle(Ast, Ctx) ->
  kapok_error:form_error(token_meta(Ast), ?m(Ctx, file), ?MODULE, {invalid_expression, {Ast}}).

%% namespace

handle_ns(Meta, [{C1, _, _} = Ast, {C2, _, Doc} | T], Ctx) when ?is_dot_id(C1), ?is_string(C2) ->
  handle_ns(Meta, Ast, Doc, T, Ctx);
handle_ns(Meta, [{C1, _, _} = Ast | T], Ctx) when ?is_dot_id(C1) ->
  handle_ns(Meta, Ast, T, Ctx);
handle_ns(Meta, _T, Ctx) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {invalid_body, {'ns'}}).

handle_ns(Meta, Ast, Clauses, Ctx) ->
  handle_ns(Meta, Ast, empty_doc(), Clauses,  Ctx).
handle_ns(_Meta, Ast, _Doc, Clauses, Ctx) ->
  Name = dot_id_name(Ast),
  Ctx1 = case ?m(Ctx, namespace) of
           nil ->
             Ctx#{namespace => Name};
           Name ->
             Ctx;
           NS ->
             %% build the previous namespace before entering the new namespace
             build_namespace(NS, Ctx),
             Ctx#{namespace => Name}
         end,
  kapok_symbol_table:add_namespace(Name),
  {_, TCtx1} = lists:mapfoldl(fun handle_ns_clause/2, Ctx1, Clauses),
  case get_compiler_opt(internal) of
    true -> TCtx1;
    false -> add_default_uses(TCtx1)
  end.

handle_ns_clause({list, _, [{identifier, _, 'require'} | T]}, Ctx) ->
  handle_require_clause(T, Ctx);
handle_ns_clause({list, _, [{identifier, _, 'use'} | T]}, Ctx) ->
  handle_use_clause(T, Ctx).

%% require
handle_require_clause(List, Ctx) when is_list(List) ->
  lists:mapfoldl(fun handle_require_element/2, Ctx, List).

handle_require_element({Category, Meta, Id}, Ctx) when ?is_local_id(Category) ->
  {Id, kapok_ctx:add_require(Meta, Ctx, Id)};
handle_require_element({dot, Meta, _} = Dot, Ctx) ->
  Name = kapok_parser:dot_fullname(Dot),
  {Name, kapok_ctx:add_require(Meta, Ctx, Name)};
handle_require_element({Category, Meta, Args}, Ctx) when ?is_list(Category) ->
  case Args of
    [{Category, _, _} = Ast, {keyword, _, 'as'}, {identifier, _, Id}] when ?is_local_id(Category) ->
      {Name, TCtx} = handle_require_element(Ast, Ctx),
      {Name, kapok_ctx:add_require(Meta, TCtx, Id, Name)};
    [{dot, _, _} = Ast, {keyword, _, 'as'}, {identifier, _, Id}] ->
      {Name, TCtx} = handle_require_element(Ast, Ctx),
      {Name, kapok_ctx:add_require(Meta, TCtx, Id, Name)};
    _ ->
      kapok_error:compile_error(Meta, ?m(Ctx, file), "invalid require expression ~p", [Args])
  end.

%% use
handle_use_clause(List, Ctx) when is_list(List) ->
  lists:mapfoldl(fun handle_use_element/2, Ctx, List).

handle_use_element({Category, Meta, Id}, Ctx) when ?is_local_id(Category) ->
  Ctx1 = kapok_ctx:add_require(Meta, Ctx, Id),
  Ctx2 = kapok_ctx:add_use(Meta, Ctx1, Id),
  {Id, Ctx2};
handle_use_element({dot, Meta, _} = Dot, Ctx) ->
  Name = kapok_parser:dot_fullname(Dot),
  Ctx1 = kapok_ctx:add_require(Meta, Ctx, Name),
  Ctx2 = kapok_ctx:add_use(Meta, Ctx1, Name),
  {Name, Ctx2};
handle_use_element({Category, Meta, Args}, Ctx) when ?is_list(Category) ->
  case Args of
    [{C, _, _} = Ast | T] when ?is_local_id(C) ->
      {Name, Ctx1} = handle_use_element(Ast, Ctx),
      handle_use_element_arguments(Meta, Name, T, Ctx1);
    [{dot, _, _} = Ast | T] ->
      {Name, Ctx1} = handle_use_element(Ast, Ctx),
      handle_use_element_arguments(Meta, Name, T, Ctx1);
    _ ->
      kapok_error:compile_error(Meta, ?m(Ctx, file), "invalid use expression ~p", [Args])
  end.

handle_use_element_arguments(Meta, Name, Args, Ctx) ->
  GArgs = group_arguments(Meta, Args, Ctx),
  handle_use_element_arguments(Meta, Name, nil, GArgs, Ctx).
handle_use_element_arguments(_Meta, Name, _, [], Ctx) ->
  {Name, Ctx};
handle_use_element_arguments(Meta, Name, Meta1, [{{keyword, _, 'as'}, {identifier, _, Id}} | T],
                             Ctx) ->
  handle_use_element_arguments(Meta, Name, Meta1, T, kapok_ctx:add_require(Meta, Ctx, Id, Name));
handle_use_element_arguments(Meta, Name, _, [{{keyword, Meta1, 'exclude'}, {_, Meta2, Args}} | T],
                             Ctx) ->
  Functions = parse_functions(Meta2, Args, Ctx),
  Ctx1 = kapok_ctx:add_use(Meta1, Ctx, Name, 'exclude', Functions),
  handle_use_element_arguments(Meta, Name, Meta1, T, Ctx1);
handle_use_element_arguments(Meta, Name, nil, [{{keyword, Meta1, 'only'}, {_, Meta2, Args}} | T],
                             Ctx) ->
  Functions = parse_functions(Meta2, Args, Ctx),
  Ctx1 = kapok_ctx:add_use(Meta1, Ctx, Name, 'only', Functions),
  handle_use_element_arguments(Meta, Name, nil, T, Ctx1);
handle_use_element_arguments(_Meta, _Name, Meta1, [{{keyword, Meta2, 'only'}, {_, _, _}} | _T],
                             Ctx) ->
  kapok_error:compile_error(Meta2,
                            ?m(Ctx, file),
                            "invalid usage of :only with :exclude present at line: ~p",
                            [?line(Meta1)]);
handle_use_element_arguments(Meta, Name, Meta1, [{{keyword, _, 'rename'}, {_, _, Args}} | T],
                             Ctx) ->
  Aliases = parse_function_aliases(Meta, Args, Ctx),
  Ctx1 = kapok_ctx:add_use(Meta, Ctx, Name, 'rename', Aliases),
  handle_use_element_arguments(Meta, Name, Meta1, T, Ctx1);
handle_use_element_arguments(_Meta, _Name, _, [Ast | _T], Ctx) ->
  kapok_error:compile_error(token_meta(Ast), ?m(Ctx, file), "invalid use argument ~p", [Ast]).

%% Check whether the default namespaces is used. Add them if they are not declared in use clause.
add_default_uses(Ctx) ->
  lists:foldl(fun(Ns, #{uses := Uses} = C) ->
                  case orddict:is_key(Ns, Uses) of
                    true ->
                      C;
                    false ->
                      {_, E1} = handle_use_element({identifier, [], Ns}, C),
                      E1
                  end
              end,
              Ctx,
              kapok_dispatch:default_uses()).

dot_id_name({'dot', _, _} = Ast) ->
  kapok_parser:dot_fullname(Ast);
dot_id_name({'identifier', _, Arg}) ->
  Arg.

%% defns
handle_def_ns(Meta, Kind, [{C, _, _} = NameAst | T], Ctx) when ?is_dot_id(C) ->
  handle_def_ns(Meta, Kind, NameAst, T, Ctx);
handle_def_ns(Meta, Kind, _T, Ctx) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {invalid_body, {Kind}}).

handle_def_ns(Meta, Kind, NameAst, [{C, _, _} = DocAst | T], Ctx) when ?is_string(C) ->
  handle_def_ns(Meta, Kind, NameAst, DocAst, T, Ctx);
handle_def_ns(Meta, Kind, _NameAst, [], Ctx) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {invalid_body, {Kind}});
handle_def_ns(Meta, Kind, NameAst, T, Ctx) ->
  handle_def_ns(Meta, Kind, NameAst, empty_doc(), T, Ctx).

handle_def_ns(Meta, _Kind, NameAst, DocAst, [{list, _, NSArgs} | T], #{namespace := NS} = Ctx) ->
  Name = dot_id_name(NameAst),
  Ctx1 = handle_ns(Meta, [NameAst, DocAst | NSArgs], Ctx#{namespace => Name}),
  Ctx2 = lists:foldl(fun handle/2, Ctx1, T),
  build_namespace(Name, Ctx2),
  Ctx2#{namespace => NS}.

%% definitions
handle_def(Meta, Kind, [{identifier, _, Name}  | T], Ctx) ->
  handle_def(Meta, Kind, Name, T, Ctx);
handle_def(Meta, Kind, _T, Ctx) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {invalid_body, {Kind}}).

handle_def(Meta, Kind, _Name, [], Ctx) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {invalid_body, {Kind}});
handle_def(Meta, Kind, Name, T, Ctx) when ?is_def_alias(Kind) ->
  handle_def_alias(Meta, Kind, Name, T, Ctx);
handle_def(Meta, Kind, Name, [{literal_list, _, _} = Args | T], Ctx) ->
  handle_def_with_args(Meta, Kind, Name, Args, T, Ctx);
handle_def(Meta, Kind, Name, [{C, _, _} = Doc | T], Ctx) when ?is_string(C) ->
  handle_def_with_doc(Meta, Kind, Name, Doc, T, Ctx);
handle_def(Meta, Kind, Name, Exprs, Ctx) ->
  handle_def_with_doc(Meta, Kind, Name, empty_doc(), Exprs, Ctx).

handle_def_alias(Meta, _Kind, Alias,
                 [{list, _, [{identifier, _, Fun}, {number, _, Arity}]}, {C, _, _} = _Doc], Ctx)
    when ?is_string(C) ->
  %% TODO add doc
  do_handle_def_alias(Meta, Alias, {Fun, Arity}, Ctx);
handle_def_alias(Meta, _Kind, Alias, [{list, _, [{identifier, _, Fun}, {number, _, Arity}]}], Ctx) ->
  do_handle_def_alias(Meta, Alias, {Fun, Arity}, Ctx);
handle_def_alias(Meta, _Kind, Alias, [{identifier, _, Fun}, {C, _, _} = _Doc], Ctx)
    when ?is_string(C) ->
  %% TODO add doc
  do_handle_def_alias(Meta, Alias, Fun, Ctx);
handle_def_alias(Meta, _Kind, Alias, [{identifier, _, Fun}], Ctx) ->
  do_handle_def_alias(Meta, Alias, Fun, Ctx);
handle_def_alias(Meta, _Kind, Alias, Ast, Ctx) ->
  Error = {invalid_defalias_expression, {Alias, Ast}},
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, Error).

do_handle_def_alias(Meta, Alias, Original, Ctx) ->
  Namespace = ?m(Ctx, namespace),
  case kapok_symbol_table:add_alias(Namespace, Alias, Original) of
    not_exist ->
      Error = {nonexistent_original_for_alias, {Alias, Original}},
      kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, Error);
    ok ->
      ok
  end,
  Ctx.

handle_def_with_args(Meta, Kind, Name, Args, [{list, _, [{identifier, _, 'when'} | _]} = Guard | T],
                     Ctx) ->
  handle_def_with_args(Meta, Kind, Name, Args, Guard, T, Ctx);
handle_def_with_args(Meta, Kind, Name, Args, T, Ctx) ->
  handle_def_with_args(Meta, Kind, Name, Args, [], T, Ctx).
handle_def_with_args(Meta, Kind, Name, Args, Guard, [{C, _, _} = _Doc | Body], Ctx)
    when ?is_string(C) ->
  %% TODO add doc
  handle_def_clause(Meta, Kind, Name, Args, Guard, Body, Ctx);
handle_def_with_args(Meta, Kind, Name, Args, Guard, Body, Ctx) ->
  handle_def_clause(Meta, Kind, Name, Args, Guard, Body, Ctx).

handle_def_with_doc(Meta, Kind, Name, _Doc, Exprs, Ctx) ->
  %% TODO add doc
  handle_def_exprs(Meta, Kind, Name, Exprs, Ctx).

empty_doc() ->
  {binary_string, [], <<"">>}.

handle_def_exprs(_Meta, Kind, Name, Exprs, Ctx) ->
  lists:foldl(fun (Expr, C) -> handle_def_expr(Kind, Name, Expr, C) end, Ctx, Exprs).

handle_def_expr(Kind,
                Name,
                {list, Meta, [{literal_list, _, _} = Args,
                              {list, _, [{identifier, _, 'when'} | _]} = Guard | Body]},
                Ctx) ->
  handle_def_clause(Meta, Kind, Name, Args, Guard, Body, Ctx);
handle_def_expr(Kind, Name, {list, Meta, [{literal_list, _, _} = Args | Body]}, Ctx) ->
  handle_def_clause(Meta, Kind, Name, Args, [], Body, Ctx);
handle_def_expr(Kind, Name, Expr, Ctx) ->
  Error = {invalid_def_expression, {Kind, Name, Expr}},
  kapok_error:form_error(token_meta(Expr), ?m(Ctx, file), ?MODULE, Error).

prepare_macro_vars(Meta, Ctx) ->
  %% add macro env variables `_&form` and `_&ctx`
  Ast = ?m(Ctx, def_ast),
  Context = #{file => ?m(Ctx, file), meta => Meta},
  Vars = [{identifier, Meta, '_&ctx'}, kapok_trans:quote(Meta, Context),
          {identifier, Meta, '_&form'}, kapok_trans:quote(Meta, Ast)],
  kapok_trans_special_form:translate_let_args(Vars, Ctx).

handle_def_clause(Meta, Kind, Name, Args, Guard, Body, #{def_fap := FAP} = Ctx) ->
  %% TODO add doc
  Namespace = ?m(Ctx, namespace),
  {TF, Ctx1} = kapok_trans:translate(Name, kapok_ctx:push_scope(Ctx)),
  {PrepareMacroEnv, TCtx} = case Kind of
                              'defmacro' -> prepare_macro_vars(Meta, Ctx1);
                              _ -> {[], Ctx1}
                            end,
  case parse_parameters(Args, TCtx) of
    [{normal, _, NormalArgs}] ->
      {TArgs, CCtx} = kapok_trans:translate_def_args(NormalArgs, TCtx),
      ParameterType = 'normal',
      PrepareBody = [];
    [{normal, _, NormalArgs}, {keyword_optional, _, OptionalParameters}] ->
      {TNormalArgs, TCtx1} = kapok_trans:translate_def_args(NormalArgs, TCtx),
      {TOptionalParameters, CCtx} = translate_parameter_with_default(OptionalParameters, TCtx1),
      ParameterType = 'normal',
      TArgs = TNormalArgs ++ get_optional_args(TOptionalParameters),
      PrepareBody = [],
      add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs, TOptionalParameters, CCtx);
    [{normal, _, NormalArgs}, {keyword_rest, _, RestArgs}] ->
      {TNormalArgs, TCtx1} = kapok_trans:translate_def_args(NormalArgs, TCtx),
      {TRestArgs, CCtx} = kapok_trans:translate_def_args(RestArgs, TCtx1),
      ParameterType = 'rest',
      TArgs = TNormalArgs ++ TRestArgs,
      PrepareBody = [],
      add_rest_clause(Meta, Kind, Namespace, Name, TF, TNormalArgs, CCtx);
    [{normal, _, NormalArgs}, {keyword_key, Meta1, KeyParameters}] ->
      {TNormalArgs, TCtx1} = kapok_trans:translate_def_args(NormalArgs, TCtx),
      {TKeyParameters, TCtx2} = translate_parameter_with_default(KeyParameters, TCtx1),
      ParameterType = 'key',
      {TMapArg, TCtx3} = kapok_trans:translate_def_arg({identifier,
                                                        Meta1,
                                                        kapok_utils:gensym("KV")},
                                                       TCtx2),
      TArgs = TNormalArgs ++ [TMapArg],

      %% retrieve and map all key values from map argument to variables
      {PrepareBody, CCtx} = kapok_trans:map_vars(Meta, TMapArg, TKeyParameters, TCtx3),
      add_key_clause(Meta, Kind, Namespace, Name, TF, TNormalArgs, CCtx);
    [{normal, _, NormalArgs}, {keyword_optional, _, OptionalParameters},
     {keyword_rest, RestArgs}] ->
      {TNormalArgs, TCtx1} = kapok_trans:translate_def_args(NormalArgs, TCtx),
      {TOptionalParameters, TCtx2} = translate_parameter_with_default(OptionalParameters, TCtx1),
      {TRestArgs, CCtx} = kapok_trans:translate_def_args(RestArgs, TCtx2),
      ParameterType = 'rest',
      TNonRestArgs = TNormalArgs ++ get_optional_args(TOptionalParameters),
      TArgs = TNonRestArgs ++ TRestArgs,
      PrepareBody = [],
      add_rest_clause(Meta, Kind, Namespace, Name, TF, TNonRestArgs, CCtx),
      add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs, TOptionalParameters, CCtx)
  end,
  Arity = length(TArgs),
  Ctx6 = CCtx#{def_fap => {Name, Arity, ParameterType}},
  %% TODO add def_fap for other clauses(maybe do this when impl delay compilation stuff?)
  {TGuard, TCtx7} = kapok_trans:translate_guard(Guard, Ctx6),
  {TBody, TCtx8} = kapok_trans:translate_body(Meta, Body, TCtx7),
  Clause = {clause, ?line(Meta), TArgs, TGuard, PrepareMacroEnv ++ PrepareBody ++ TBody},
  kapok_symbol_table:add_def(Namespace, Kind, Name, Arity, ParameterType, Clause),
  kapok_ctx:pop_scope(TCtx8#{def_fap => FAP}).

%% parse parameters
parse_parameters({Category, _, Args}, Ctx) when Category == literal_list ->
  parse_parameters(Args, Ctx);
parse_parameters(Args, Ctx) when is_list(Args) ->
  L = parse_parameters(Args, [], {normal, [], []}, Ctx),
  lists:reverse(L).

parse_parameters([], Acc, {Previous, Meta, Args}, _Ctx) ->
  [{Previous, Meta, lists:reverse(Args)} | Acc];

parse_parameters([{Category, Meta, _} = Token], _Acc, {_Previous, _Meta, _Args}, Ctx)
    when ?is_parameter_keyword(Category) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {dangling_parameter_keyword, {Token}});

parse_parameters([{keyword_optional, Meta, _} | T], Acc, {normal, Meta1, Args}, Ctx) ->
  parse_parameters(T, [{normal, Meta1, lists:reverse(Args)} | Acc], {keyword_optional, Meta, []},
                   Ctx);
parse_parameters([{keyword_optional, Meta, _} = Token | _T], _Acc, {Previous, Meta1, _Args}, Ctx) ->
  Error = {invalid_postion_of_parameter_keyword, {Token, {Previous, Meta1}}},
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, Error);

parse_parameters([{keyword_rest, Meta, _} | T], Acc, {normal, Meta1, Args}, Ctx) ->
  parse_parameters(T, [{normal, Meta1, lists:reverse(Args)} | Acc], {keyword_rest, Meta, []}, Ctx);
parse_parameters([{keyword_rest, Meta, _} | T], Acc, {keyword_optional, Meta1, Args}, Ctx) ->
  Last = {keyword_optional, Meta1, lists:reverse(Args)},
  parse_parameters(T, [Last | Acc], {keyword_rest, Meta, []}, Ctx);
parse_parameters([{keyword_rest, Meta, _} = Token | _T], _Acc, {Previous, Meta1, _Args}, Ctx) ->
  Error = {invalid_postion_of_parameter_keyword, {Token, {Previous, Meta1}}},
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, Error);

parse_parameters([{keyword_key, Meta, _} | T], Acc, {normal, Meta1, Args}, Ctx) ->
  parse_parameters(T, [{normal, Meta1, lists:reverse(Args)} | Acc], {keyword_key, Meta, []}, Ctx);
parse_parameters([{keyword_key, Meta, _} | _T], _Acc, {Previous, Meta1, _Args}, Ctx) ->
  Error = {invalid_postion_of_parameter_keyword, {keyword_key, {Previous, Meta1}}},
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, Error);

parse_parameters([H | T], Acc, {normal, Meta, Args}, Ctx) ->
  parse_parameters(T, Acc, {normal, Meta, [H | Args]}, Ctx);

parse_parameters([{list, _Meta, [Expr, Default]} | T], Acc, {keyword_optional, Meta1, Args}, Ctx) ->
  parse_parameters(T, Acc, {keyword_optional, Meta1, [{Expr, Default} | Args]}, Ctx);
parse_parameters([{identifier, Meta, _} = Id | T], Acc, {keyword_optional, Meta1, Args}, Ctx) ->
  parse_parameters(T, Acc, {keyword_optional, Meta1, [{Id, {atom, Meta, 'nil'}} | Args]}, Ctx);
parse_parameters([H | _T], _Acc, {keyword_optional, Meta1, _Args}, Ctx) ->
  Error = {invalid_parameter, {H, {keyword_optional, Meta1}}},
  kapok_error:form_error(token_meta(H), ?m(Ctx, file), ?MODULE, Error);

parse_parameters([H | _T], _Acc, {keyword_rest, Meta1, Args}, Ctx) when Args /= [] ->
  Error = {too_many_parameters_for_keyword, {H, {keyword_rest, Meta1}}},
  kapok_error:form_error(token_meta(H), ?m(Ctx, file), ?MODULE, Error);

parse_parameters([{list, _Meta, [{identifier, _, _} = Expr, Default]} | T],
                 Acc,
                 {keyword_key, Meta1, Args},
                 Ctx) ->
  parse_parameters(T, Acc, {keyword_key, Meta1, [{Expr, Default} | Args]}, Ctx);
parse_parameters([{identifier, Meta, _} = Id | T], Acc, {keyword_key, Meta1, Args}, Ctx) ->
  parse_parameters(T, Acc, {keyword_key, Meta1, [{Id, {atom, Meta, 'nil'}} | Args]}, Ctx);
parse_parameters([H | _T], _Acc, {keyword_key, Meta1, _Args}, Ctx) ->
  Error = {invalid_parameter, {H, {keyword_key, Meta1}}},
  kapok_error:form_error(token_meta(H), ?m(Ctx, file), ?MODULE, Error);

parse_parameters([H | T], Acc, {Previous, Meta1, Args}, Ctx) ->
  parse_parameters(T, Acc, {Previous, Meta1, [H | Args]}, Ctx).

get_optional_args(OptionalParameters) ->
  lists:map(fun({Expr, _Default}) -> Expr end, OptionalParameters).

translate_parameter_with_default(Parameters, Ctx) ->
  {L, TCtx} = lists:foldl(fun({Expr, Default}, {Acc, C}) ->
                              {TExpr, C1} = kapok_trans:translate_def_arg(Expr, C),
                              {TDefault, C2} = kapok_trans:translate_def_arg(Default, C1),
                              {[{TExpr, TDefault} | Acc], C2}
                          end,
                          {[], Ctx},
                          Parameters),
  {lists:reverse(L), TCtx}.

add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs, TOptionalParameters, Ctx) ->
  R = lists:reverse(TOptionalParameters),
  do_add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs, R, Ctx).
do_add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs, TReversedOptionalParameters,
                        Ctx) ->
  case TReversedOptionalParameters of
    [{_Expr, Default} | Left] ->
      TArgs = TNormalArgs ++ get_optional_args(lists:reverse(Left)),
      add_redirect_clause(Meta, Kind, Namespace, Name, TF, TArgs, Default),
      do_add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs, Left, Ctx);
    [] ->
      ok
  end.

add_rest_clause(Meta, Kind, Namespace, Name, TF, TNormalArgs, Ctx) ->
  {TListArgs, _} = kapok_trans:translate({literal_list, [], []}, Ctx),
  add_redirect_clause(Meta, Kind, Namespace, Name, TF, TNormalArgs, TListArgs).

add_key_clause(Meta, Kind, Namespace, Name, TF, TNormalArgs, Ctx) ->
  {TMapArg, _} = kapok_trans:translate({map, [], []}, Ctx),
  add_redirect_clause(Meta, Kind, Namespace, Name, TF, TNormalArgs, TMapArg).

add_redirect_clause(Meta, Kind, Namespace, Name, TF, TNormalArgs, Extra) ->
  %% redirect normal call(without &rest/&key argument) to the clause with an extra default argument
  Arity = length(TNormalArgs),
  TArgs = TNormalArgs ++ [Extra],
  TBody = [{call, ?line(Meta), TF, TArgs}],
  kapok_symbol_table:add_def(Namespace, Kind, Name, Arity, 'normal',
                             {clause, ?line(Meta), TNormalArgs, [], TBody}).

%% namespace
build_namespace(Namespace, Ctx) ->
  {Forms, Ctx1} = kapok_symbol_table:namespace_forms(Namespace, Namespace, Ctx),
  kapok_erl:module(Forms, [], Ctx1, fun write_compiled/2).

write_compiled(Module, Binary) ->
  %% write compiled binary to dest file
  case kapok_env:get(outdir) of
    nil ->
      try
        Module:main()
      catch
        error:undef -> ok
      end;
    Outdir ->
      binary_to_path({Module, Binary}, Outdir)
  end.

binary_to_path({ModuleName, Binary}, Outdir) ->
  Path = filename:join(Outdir, atom_to_list(ModuleName) ++ ?BEAM_FILE_SUFFIX),
  ok = file:write_file(Path, Binary),
  Path.

%% Helpers

group_arguments(Meta, Args, Ctx) ->
  group_arguments(Meta, Args, orddict:new(), Ctx).
group_arguments(_Meta, [], Acc, _Ctx) ->
  lists:map(fun({_, KV}) -> KV end, orddict:to_list(Acc));
group_arguments(Meta, [{keyword, _, 'as'} = K, {identifier, _, _} = V | T], Acc, Ctx) ->
  group_arguments(Meta, T, orddict:store('as', {K, V}, Acc), Ctx);
group_arguments(Meta, [{keyword, _, 'only'} = K, {C, _, _} = V | T], Acc, Ctx)
    when ?is_list(C) ->
  group_arguments(Meta, T, orddict:store('only', {K, V}, Acc), Ctx);
group_arguments(Meta, [{keyword, _, 'exclude'} = K, {C, _, _} = V | T], Acc, Ctx)
    when ?is_list(C) ->
  group_arguments(Meta, T, orddict:store('exclude', {K, V}, Acc), Ctx);
group_arguments(Meta, [{keyword, _, 'rename'} = K, {C, _, _} = V | T], Acc, Ctx)
    when ?is_list(C) ->
  group_arguments(Meta, T, orddict:store('rename', {K, V}, Acc), Ctx);
group_arguments(Meta, Args, _Acc, Ctx) ->
  kapok_error:compile_error(Meta, ?m(Ctx, file), "invalid use arguments: ~p~n", [Args]).

parse_functions(Meta, Args, Ctx) ->
  Fun = fun({list, _, [{identifier, _, Id}, {number, _, Integer}]}) when is_integer(Integer) ->
            {Id, Integer};
           ({Category, _, Id}) when ?is_local_id(Category) ->
            Id;
           (Other) ->
            kapok_error:compile_error(Meta, ?m(Ctx, file), "invalid function: ~p", [Other])
        end,
  ordsets:from_list(lists:map(Fun, Args)).


parse_function_aliases(Meta, Args, Ctx) ->
  Fun = fun({Category, _, [{list, _, [{identifier, _, Id},
                                      {number, _, Integer}]},
                           {identifier, _, Alias}]})
              when ?is_list(Category), is_integer(Integer) ->
            {Alias, {Id, Integer}};
           ({Category, _, [{C1, _, Id}, {C2, _, Alias}]})
              when ?is_list(Category), ?is_local_id(C1), ?is_local_id(C2) ->
            {Alias, Id};
           (Other) ->
            kapok_error:compile_error(Meta, ?m(Ctx, file), "invalid rename arguments: ~p", [Other])
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
format_error({dangling_parameter_keyword, {Token}}) ->
  io_lib:format("dangling ~s without argument", [token_text(Token)]);
format_error({invalid_def_expression, {Kind, Name, Expr}}) ->
  io_lib:format("invalid expression ~p for ~s ~s", [Expr, Kind, Name]);
format_error({invalid_postion_of_parameter_keyword, {Token, Previous}}) ->
  io_lib:format("invalid ~s with ~s ahead at line ~B", [token_text(Token),
                                                        token_text(Previous),
                                                        ?line(token_meta(Previous))]);
format_error({invalid_parameter, {P, Token}}) ->
  io_lib:format("invalid parameter ~p for ~s at line ~B",
                [P, token_text(Token), ?line(token_meta(Token))]);
format_error({too_many_parameters_for_keyword, {P, Token}}) ->
  io_lib:format("too many parameters ~p for ~s", [P, token_text(Token)]).
