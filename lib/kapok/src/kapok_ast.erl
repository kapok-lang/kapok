%% ast handlings, which include expanding/translating the kapok ast
-module(kapok_ast).
-export([compile/2,
         add_uses/2,
         build_namespace/6,
         format_error/1,
         empty_doc/0]).
-import(kapok_scanner, [token_text/1, token_meta/1]).
-import(kapok_parser, [is_plain_dot/1, plain_dot_name/1]).
-include("kapok.hrl").

compile(Ast, Ctx) when is_list(Ast) ->
  Ctx1 = lists:foldl(fun compile/2, Ctx, Ast),
  build_namespace(?m(Ctx1, namespace), Ctx1);
compile(Ast, Ctx) ->
  {EAst, ECtx} = kapok_macro:expand(Ast, Ctx),
  handle(EAst, ECtx).

%% Sometimes we need to expand a top level macro call to a list of ast,
%% in this case, we need to handle the returned all these asts in sequence.
handle(List, Ctx) when is_list(List) ->
  lists:foldl(fun handle/2, Ctx, List);

handle({list, Meta, [{identifier, _, Id} | T]}, Ctx) when ?is_ns(Id) ->
  handle_ns(Meta, T, Ctx#{def_kind => Id});
handle({list, Meta, [{identifier, _, Id} | T]}, Ctx) when ?is_def_ns(Id) ->
  handle_def_ns(Meta, Id, T, Ctx#{def_kind => Id});
handle({list, Meta, [{identifier, _, Id} | T]} = Ast, Ctx) when ?is_def(Id) ->
  handle_def(Meta, Id, T, Ctx#{def_kind => Id, def_ast => Ast});
handle({list, _Meta, [{identifier, _, Id} | _T]} = Ast, Ctx) when ?is_attr(Id) ->
  {TAttr, TCtx} = kapok_trans:translate(Ast, Ctx#{def_kind => Id}),
  Namespace = ?m(Ctx, namespace),
  kapok_symbol_table:new_form(Namespace, TAttr),
  TCtx;
handle(Ast, Ctx) ->
  kapok_error:form_error(token_meta(Ast), ?m(Ctx, file), ?MODULE, {invalid_expression, {Ast}}).

%% metadata

update_metadata(Metadata, #{metadata := M} = Ctx) ->
  M1 = maps:merge(M, Metadata),
  {M, Ctx#{metadata => M1}}.

restore_metadata(Metadata, Ctx) ->
  Ctx#{metadata => Metadata}.

%% namespace

handle_ns(Meta, [{C1, _, _} = Ast, {C2, _, Doc} | T], Ctx) when ?is_local_id_or_dot(C1), ?is_string(C2) ->
  handle_ns(Meta, Ast, Doc, T, Ctx);
handle_ns(Meta, [{C1, _, _} = Ast | T], Ctx) when ?is_local_id_or_dot(C1) ->
  handle_ns(Meta, Ast, T, Ctx);
handle_ns(Meta, T, Ctx) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {invalid_body, {'ns', T}}).

handle_ns(Meta, Ast, Clauses, Ctx) ->
  handle_ns(Meta, Ast, empty_doc(), Clauses,  Ctx).
handle_ns(_Meta, Ast, _Doc, Clauses, Ctx) ->
  case is_plain_dot(Ast) of
    true ->
      ok;
    false ->
      kapok_error:compile_error(token_meta(Ast), ?m(Ctx, file),
                                "invalid dot expression ~p for ns", [Ast])
  end,
  Name = plain_dot_name(Ast),
  Ctx1 = case ?m(Ctx, namespace) of
           nil ->
             Ctx#{namespace => Name};
           Name ->
             Ctx;
           NS ->
             %% build the previous namespace before entering the new namespace
             build_namespace(NS, Ctx),
             %% create a new context for the new namespace
             File = maps:get(file, Ctx),
             kapok_ctx:ctx_for_eval([{namespace, Name}, {file, File}, {line, 1}])
         end,
  kapok_symbol_table:new_namespace(Name),
  {_, TCtx1} = lists:mapfoldl(fun handle_ns_clause/2, Ctx1, Clauses),
  TCtx1.

handle_ns_clause({list, _, [{identifier, _, 'require'} | T]}, Ctx) ->
  handle_require_clause(T, Ctx);
handle_ns_clause({list, _, [{identifier, _, 'use'} | T]}, Ctx) ->
  handle_use_clause(T, Ctx).

%% require
handle_require_clause(List, Ctx) when is_list(List) ->
  lists:mapfoldl(fun handle_require_element/2, Ctx, List).

handle_require_element({Category, Meta, Id}, Ctx) when ?is_id(Category) ->
  {Id, kapok_ctx:add_require(Meta, Ctx, Id)};
handle_require_element({dot, Meta, _} = Dot, Ctx) ->
  case is_plain_dot(Dot) of
    true ->
      Name = plain_dot_name(Dot),
      {Name, kapok_ctx:add_require(Meta, Ctx, Name)};
    false ->
      kapok_error:compile_error(Meta, ?m(Ctx, file), "invalid dot expression ~p for require", [Dot])
  end;
handle_require_element({C1, _, [{identifier, _, Id}, {C2, _, Args}]}, Ctx)
    when ?is_list(C1), Id == ?STDLIB_NS, ?is_list(C2) ->
  Fun = fun({C3, C3Meta, _} = A, Ctx0) when ?is_id_or_dot(C3) ->
            case is_plain_dot(A) of
              true ->
                Name = plain_dot_name(A),
                Alias = {list, C3Meta, [{dot, C3Meta, {{identifier, C3Meta, ?STDLIB_NS},
                                                       {identifier, C3Meta, Name}}},
                                        {keyword, C3Meta, 'as'},
                                        {identifier, C3Meta, Name}]},
                {Name1, Ctx1} = handle_require_element(Alias, Ctx0),
                {Name1, Ctx1};
              false ->
                kapok_error:compile_error(C3Meta, ?m(Ctx, file), "invalid expression ~p for require", [A])
            end;
           ({C3, C3Meta, C3Args} = A, Ctx0) when ?is_list(C3) ->
            case C3Args of
              [{C4, C4Meta, _} = Id1 | T] when ?is_id_or_dot(C4) ->
                Name = plain_dot_name(Id1),
                Dot = {dot, C4Meta, {{identifier, C4Meta, ?STDLIB_NS},
                                     {identifier, C4Meta, Name}}},
                {_, Ctx1} = handle_require_element({C3, C3Meta, [Dot | T]}, Ctx0),
                Alias = {list, C3Meta, [Dot,
                                        {keyword, C3Meta, 'as'},
                                        {identifier, C3Meta, Name}]},
                handle_require_element(Alias, Ctx1);
              _ ->
                kapok_error:compile_error(C3Meta, ?m(Ctx, file), "invalid require expression ~p", [A])
            end
        end,
  lists:mapfoldl(Fun,
                 Ctx,
                 Args);
handle_require_element({Category, Meta, Args}, Ctx) when ?is_list(Category) ->
  case Args of
    [{identifier, _, _} = Ast, {keyword, _, 'as'}, {identifier, _, Id}] ->
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

handle_use_element({Category, Meta, Id}, Ctx) when ?is_id(Category) ->
  Ctx1 = kapok_ctx:add_require(Meta, Ctx, Id),
  Ctx2 = kapok_ctx:add_use(Meta, Ctx1, Id),
  {Id, Ctx2};
handle_use_element({dot, Meta, _} = Dot, Ctx) ->
  case is_plain_dot(Dot) of
    true ->
      Name = plain_dot_name(Dot),
      Ctx1 = kapok_ctx:add_require(Meta, Ctx, Name),
      Ctx2 = kapok_ctx:add_use(Meta, Ctx1, Name),
      {Name, Ctx2};
    false ->
      kapok_error:compile_error(Meta, ?m(Ctx, file), "invalid dot expression ~p for use", [Dot])
  end;
handle_use_element({C1, _, [{identifier, _, Id}, {C2, _, Args}]}, Ctx)
    when ?is_list(C1), Id == ?STDLIB_NS, ?is_list(C2) ->
  Fun = fun({C3, C3Meta, _} = A, Ctx0) when ?is_id_or_dot(C3) ->
            case is_plain_dot(A) of
              true ->
                Name = plain_dot_name(A),
                Alias = {list, C3Meta, [{dot, C3Meta, {{identifier, C3Meta, ?STDLIB_NS},
                                                       {identifier, C3Meta, Name}}},
                                        {keyword, C3Meta, 'as'},
                                        {identifier, C3Meta, Name}]},
                {Name1, Ctx1} = handle_use_element(Alias, Ctx0),
                {Name1, Ctx1};
              false ->
                kapok_error:compile_error(C3Meta, ?m(Ctx, file), "invalid expression ~p for require", [A])
            end;
           ({C3, C3Meta, C3Args} = A, Ctx0) when ?is_list(C3) ->
            case C3Args of
              [{C4, C4Meta, _} = Id1 | T] when ?is_id_or_dot(C4) ->
                Name = plain_dot_name(Id1),
                Dot = {dot, C4Meta, {{identifier, C4Meta, ?STDLIB_NS},
                                     {identifier, C4Meta, Name}}},
                {_, Ctx1} = handle_use_element({C4, C4Meta, [Dot | T]}, Ctx0),
                Alias = {list, C3Meta, [Dot,
                                        {keyword, C3Meta, 'as'},
                                        {identifier, C3Meta, Name}]},
                handle_require_element(Alias, Ctx1);
              _ ->
                kapok_error:compile_error(C3Meta, ?m(Ctx, file), "invalid use expression ~p", [A])
            end
        end,
  lists:mapfoldl(Fun,
                 Ctx,
                 Args);
handle_use_element({Category, Meta, Args}, Ctx) when ?is_list(Category) ->
  case Args of
    [{C, _, _} = Ast | T] when ?is_id(C) ->
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
add_uses(Ctx, ModuleList) ->
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
              ModuleList).


%% defns
handle_def_ns(Meta, Kind, [NameAst | T], Ctx) ->
  case is_plain_dot(NameAst) of
    true ->
      handle_def_ns(Meta, Kind, NameAst, T, Ctx);
    false ->
      kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {invalid_body, {Kind, T}})
  end.

handle_def_ns(Meta, Kind, NameAst, [{C, _, _} = DocAst | T], Ctx) when ?is_string(C) ->
  handle_def_ns(Meta, Kind, NameAst, DocAst, T, Ctx);
handle_def_ns(Meta, Kind, _NameAst, [], Ctx) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {invalid_body, {Kind, []}});
handle_def_ns(Meta, Kind, NameAst, T, Ctx) ->
  handle_def_ns(Meta, Kind, NameAst, empty_doc(), T, Ctx).

handle_def_ns(Meta, _Kind, NameAst, DocAst, [{list, _, NSArgs} | T], #{namespace := NS} = Ctx) ->
  Name = plain_dot_name(NameAst),
  Ctx1 = handle_ns(Meta, [NameAst, DocAst | NSArgs], Ctx#{namespace => Name}),
  Ctx2 = lists:foldl(fun handle/2, Ctx1, T),
  build_namespace(Name, Ctx2),
  Ctx2#{namespace => NS}.

%% definitions

handle_def(Meta, Kind, [{map, _, _} = Map | T], Ctx) ->
  %% metadata
  {Metadata, Ctx1} = kapok_compiler:eval_ast(Map, Ctx),
  {Original, Ctx2} = update_metadata(Metadata, Ctx1),
  Ctx3 = handle_def(Meta, Kind, T, Ctx2),
  %% restore the original metadata later
  restore_metadata(Original, Ctx3);
handle_def(Meta, Kind, [{identifier, _, Name}  | T], Ctx) ->
  handle_def(Meta, Kind, Name, T, Ctx);
handle_def(Meta, Kind, T, Ctx) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {invalid_body, {Kind, T}}).

handle_def(Meta, Kind, _Name, [], Ctx) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {invalid_body, {Kind, []}});
handle_def(Meta, Kind, Name, T, Ctx) when ?is_def_alias(Kind) ->
  handle_def_alias(Meta, Kind, Name, T, Ctx);
handle_def(Meta, Kind, Name, [{C, _, _} = Args | T], Ctx) when ?is_parameter_list(C) ->
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
  kapok_symbol_table:new_alias(Namespace, Alias, Original, Meta),
  Ctx.

handle_def_with_args(Meta, Kind, Name, Args, [{list, _, [{keyword_when, _, _} | _]} = Guard | T],
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
                {list, Meta, [{C, _, _} = Args,
                              {list, _, [{keyword_when, _, _} | _]} = Guard | Body]},
                Ctx)
    when ?is_parameter_list(C)->
  handle_def_clause(Meta, Kind, Name, Args, Guard, Body, Ctx);
handle_def_expr(Kind, Name, {list, Meta, [{C, _, _} = Args | Body]}, Ctx)
    when ?is_parameter_list(C) ->
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

handle_def_clause(Meta, Kind, Name, Args, Guard, Body, #{def_fap := PreviousFAP} = Ctx) ->
  %% TODO add doc
  Namespace = ?m(Ctx, namespace),
  {TF, Ctx1} = kapok_trans:translate(Name, kapok_ctx:push_scope(Ctx)),
  {PrepareMacroEnv, TCtx} = case Kind of
                              K when ?is_def_macro(K) -> prepare_macro_vars(Meta, Ctx1);
                              _ -> {[], Ctx1}
                            end,
  case parse_parameters(Args, TCtx) of
    [{normal, _, NormalArgs}] ->
      {TArgs, CCtx} = kapok_trans:translate_def_args(NormalArgs, TCtx),
      ParameterType = 'normal',
      PrepareBody = [],
      RedirectedFAPList = [],
      AddRedirctClauses = fun() -> ok end;
    [{normal, _, NormalArgs}, {keyword_optional, _, OptionalParameters}] ->
      {TNormalArgs, TCtx1} = kapok_trans:translate_def_args(NormalArgs, TCtx),
      {TOptionalParameters, CCtx} = translate_parameter_with_default(OptionalParameters, TCtx1),
      ParameterType = 'normal',
      TArgs = TNormalArgs ++ get_optional_args(TOptionalParameters),
      PrepareBody = [],
      {_, RedirectedFAPList} = calc_optional_redirected_fap(Name, TNormalArgs,
                                                            TOptionalParameters),
      AddRedirctClauses = fun() ->
                              add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs,
                                                   TOptionalParameters, CCtx)
                          end;
    [{normal, _, NormalArgs}, {keyword_rest, _, RestArgs}] ->
      {TNormalArgs, TCtx1} = kapok_trans:translate_def_args(NormalArgs, TCtx),
      {TRestArgs, CCtx} = kapok_trans:translate_def_args(RestArgs, TCtx1),
      ParameterType = 'rest',
      TArgs = TNormalArgs ++ TRestArgs,
      PrepareBody = [],
      RedirectedFAPList = [{Name, length(TNormalArgs), 'normal'}],
      AddRedirctClauses = fun() ->
                              add_rest_clause(Meta, Kind, Namespace, Name, TF, TNormalArgs, CCtx)
                          end;
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
      RedirectedFAPList = [{Name, length(TNormalArgs), 'normal'}],
      AddRedirctClauses = fun() ->
                              add_key_clause(Meta, Kind, Namespace, Name, TF, TNormalArgs, CCtx)
                          end;
    [{normal, _, NormalArgs}, {keyword_optional, _, OptionalParameters},
     {keyword_rest, _, RestArgs}] ->
      {TNormalArgs, TCtx1} = kapok_trans:translate_def_args(NormalArgs, TCtx),
      {TOptionalParameters, TCtx2} = translate_parameter_with_default(OptionalParameters,
                                                                      TCtx1),
      {TRestArgs, CCtx} = kapok_trans:translate_def_args(RestArgs, TCtx2),
      ParameterType = 'rest',
      TNonRestArgs = TNormalArgs ++ get_optional_args(TOptionalParameters),
      {N, OptionalFAPList} = calc_optional_redirected_fap(Name, TNormalArgs,
                                                          TOptionalParameters),
      RedirectedFAPList = [{Name, N + 1, 'normal'} | OptionalFAPList],
      TArgs = TNonRestArgs ++ TRestArgs,
      PrepareBody = [],
      AddRedirctClauses = fun() ->
                              add_rest_clause(Meta, Kind, Namespace, Name, TF, TNonRestArgs, CCtx),
                              add_optional_clauses(Meta, Kind, Namespace, Name, TF, TNormalArgs,
                                                   TOptionalParameters, CCtx)
                          end
  end,
  Arity = length(TArgs),
  FAP = {Name, Arity, ParameterType},
  Ctx6 = CCtx#{def_fap => FAP},
  %% Check if there is any suspended def clause depends on this clause.
  %% If there is, re-do the compilation of the suspended clauses.
  FAPList = [FAP | RedirectedFAPList],
  {R, NewAliasFAPList} = handle_suspended_def_clauses(Namespace, Kind, FAPList, Ctx),
  %% TODO add def_fap for other clauses (maybe do this when impl delay compilation stuff?)
  {TGuard, TCtx7} = kapok_trans:translate_guard(Guard, Ctx6),
  try kapok_trans:translate_body(Meta, Body, TCtx7) of
      {TBody, TCtx8} ->
      %% add redirect FAPs and clauses
      AddRedirctClauses(),
      Clause = {clause, ?line(Meta), TArgs, TGuard, PrepareMacroEnv ++ PrepareBody ++ TBody},
      %% TODO add conflict checking for Namespace:Name/Arity and imported names.
      kapok_symbol_table:new_def(Namespace, Kind, Name, Arity, ParameterType, Meta, Clause),
      kapok_ctx:pop_scope(TCtx8#{def_fap => PreviousFAP})
  catch
    throw:{unknown_local_call, FA, FAMeta} ->
      %% clean up added fap
      case R of
        handled ->
          kapok_symbol_table:cleanup_fap_alias(Namespace, Kind, FAPList, NewAliasFAPList);
        skip ->
          ok
      end,
      %% record current def clause as suspended
      kapok_symbol_table:new_suspended_def_clause(Namespace, FA,
                                                  {FAMeta, {Meta, Kind, Name, Args, Guard, Body}}),
      kapok_ctx:pop_scope(TCtx7#{def_fap => PreviousFAP})
  end.

%% parse parameters
parse_parameters({Category, _, Args}, Ctx) when Category == literal_list ->
  parse_parameters(Args, Ctx);
%% For the case when `&` is used instead of `&rest`
parse_parameters({Category, Meta, {Head, Tail}}, Ctx) when Category == cons_list ->
  parse_parameters(Head ++ [{keyword_rest, Meta, '&rest'}, Tail], Ctx);
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

parse_parameters([{list, _Meta, [{identifier, _, Id} = Token, Default]} | T],
                 Acc,
                 {keyword_key, Meta1, Args},
                 Ctx) ->
  case Id of
    Value when Value == true; Value == false; Value == nil ->
      Error = {invalid_key, {Token}},
      kapok_error:form_error(token_meta(Token), ?m(Ctx, file), ?MODULE, Error);
    _ ->
      parse_parameters(T, Acc, {keyword_key, Meta1, [{Token, Default} | Args]}, Ctx)
  end;
parse_parameters([{identifier, Meta, Id} = Token | T], Acc, {keyword_key, Meta1, Args}, Ctx) ->
  case Id of
    Value when Value == true, Value == false, Value == nil ->
      Error = {invalid_key, {Token}},
      kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, Error);
    _ ->
      parse_parameters(T, Acc, {keyword_key, Meta1, [{Token, {atom, Meta, 'nil'}} | Args]}, Ctx)
  end;
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

calc_optional_redirected_fap(Name, TNormalArgs, TOptionalParameters) ->
  Arity = length(TNormalArgs),
  lists:foldl(fun(_X, {N, Acc0}) ->
                  Acc1 = [{Name, Arity + N, 'normal'} | Acc0],
                  {N + 1, Acc1}
              end,
              {0, []},
              TOptionalParameters).


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
  kapok_symbol_table:new_def(Namespace, Kind, Name, Arity, 'normal', Meta,
                             {clause, ?line(Meta), TNormalArgs, [], TBody}).

handle_suspended_def_clauses(Namespace, CurrentKind, FAPList, Ctx) ->
  {ToHandle, AliasFAPList} = kapok_symbol_table:check_suspended_def_clauses(
                                 Namespace, CurrentKind, FAPList),
  orddict:map(fun(_, S) ->
                  lists:map(fun({_FAMeta, {Meta, Kind, Name, Args, Guard, Body}}) ->
                                handle_def_clause(Meta, Kind, Name, Args, Guard, Body, Ctx)
                            end,
                            ordsets:to_list(S))
              end,
              ToHandle),
  R = case ToHandle of
        [] ->
          skip;
        _ ->
          handled
      end,
  {R, AliasFAPList}.

build_namespace(Namespace, ModuleName, Ctx, STOptions, ErlOptions, Callback) ->
  {Forms, Ctx1} = kapok_symbol_table:namespace_forms(Namespace, ModuleName, Ctx, STOptions),
  kapok_erl:module(Forms, ErlOptions, Ctx1, Callback).

build_namespace(Namespace, Ctx) ->
  %% Strip all private macros from the forms for the final compilation of the specified module.
  %% A private macro is meant to be called only in current module. There will be
  %% no explicit call to it any more after its full expansion. We strip their definitions
  %% from the forms for the final compilation in order to avoid unused warnings caused by them.
  STOptions = [strip_private_macro],
  ErlOptions = [],
  build_namespace(Namespace, Namespace, Ctx, STOptions, ErlOptions, fun write_compiled/2).

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
           ({Category, _, Id}) when ?is_id(Category) ->
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
              when ?is_list(Category), ?is_id(C1), ?is_id(C2) ->
            {Alias, Id};
           (Other) ->
            kapok_error:compile_error(Meta, ?m(Ctx, file), "invalid rename arguments: ~p", [Other])
        end,
  ordsets:from_list(lists:map(Fun, Args)).

%% Error
format_error({invalid_expression, {Ast}}) ->
  io_lib:format("invalid top level expression ~s", [token_text(Ast)]);
format_error({invalid_body, {Form, Left}}) ->
  io_lib:format("invalid body for form: ~p, ~p~n", [Form, Left]);
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
  io_lib:format("too many parameters ~p for ~s", [P, token_text(Token)]);
format_error({invalid_key, {Token}}) ->
  io_lib:format("invalid key ~s", [token_text(Token)]).
