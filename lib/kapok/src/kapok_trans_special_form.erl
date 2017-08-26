%% special form
-module(kapok_trans_special_form).
-export([translate_attribute/4,
         translate_let/4,
         translate_let_args/2,
         translate_do/3,
         translate_case/5,
         translate_fn/3,
         translate_fn/4,
         translate_fn/5,
         translate_fn/6,
         translate_send/4,
         translate_receive/3,
         translate_try/3,
         format_error/1
        ]).
-import(kapok_scanner, [token_meta/1]).
-import(kapok_trans, [translate/2,
                      translate_def_arg/2,
                      translate_def_args/2,
                      translate_guard/2,
                      translate_body/3]).
-include("kapok.hrl").

%% attribute
translate_attribute(Meta, A, T, Ctx) ->
  {{attribute,?line(Meta), A, T}, Ctx}.

%% let
translate_let(Meta, Args, Body, Ctx) ->
  Ctx1 = kapok_ctx:push_scope(Ctx),
  {TArgs, TCtx1} = translate_let_args(Args, Ctx1),
  {TBody, TCtx2} = translate_body(Meta, Body, TCtx1),
  BodyBlock = build_block(0, TBody),
  TCtx3 = kapok_ctx:pop_scope(TCtx2),
  {build_block(Meta, TArgs ++ [BodyBlock]), TCtx3}.

translate_let_pattern(Arg, #{context := Context} = Ctx) ->
  {TArg, TCtx} = translate(Arg, Ctx#{context => let_pattern}),
  {TArg, TCtx#{context => Context}}.

translate_let_args(Args, Ctx) ->
  translate_let_args(Args, [], Ctx).
translate_let_args([], Acc, Ctx) ->
  {lists:reverse(Acc), Ctx#{context => nil}};
translate_let_args([H], _Acc, Ctx) ->
  Error = {let_odd_forms, {H}},
  kapok_error:form_error(kapok_scanner:token_meta(H), ?m(Ctx, file), ?MODULE, Error);
translate_let_args([P1, P2 | T], Acc, Ctx) ->
  {TP1, TCtx} = translate_let_pattern(P1, Ctx),
  {TP2, TCtx1} = translate(P2, TCtx),
  translate_let_args(T, [{match, ?line(kapok_scanner:token_meta(P1)), TP1, TP2} | Acc], TCtx1).

%% do
translate_do(Meta, Exprs, Ctx) ->
  {TExprs, TCtx} = translate(Exprs, Ctx),
  {build_block(Meta, TExprs), TCtx}.

build_block(Meta, Exprs) when is_list(Meta) ->
  build_block(?line(Meta), Exprs);
build_block(Line, Exprs) when is_integer(Line) ->
  {block, Line, Exprs}.

%% case
translate_case(Meta, Expr, Clause, Left, Ctx) ->
  {TExpr, TCtx} = translate(Expr, Ctx),
  {TClause, TCtx1} = translate_case_clause(Clause, TCtx),
  {TLeft, TCtx2} = lists:mapfoldl(fun translate_case_clause/2, TCtx1, Left),
  {{'case', ?line(Meta), TExpr, [TClause | TLeft]}, TCtx2}.

translate_case_clause({list, Meta, []}, Ctx) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {empty_case_clause});
translate_case_clause({list, Meta, [_P]}, Ctx) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {missing_case_clause_body});
translate_case_clause({list, Meta, [P, {list, _, [{identifier, _, 'when'} | _]} = Guard | Body]},
                      Ctx) ->
  translate_case_clause(Meta, P, Guard, Body, Ctx);
translate_case_clause({list, Meta, [P | B]}, Ctx) ->
  translate_case_clause(Meta, P, [], B, Ctx);
translate_case_clause({C, Meta, _} = Ast, Ctx) when C /= list ->
  Error = {invalid_case_clause, {Ast}},
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, Error).

translate_case_clause(Meta, Pattern, Guard, Body, Ctx) ->
  Ctx1 = kapok_ctx:push_scope(Ctx),
  {TPattern, TCtx1} = translate_def_arg(Pattern, Ctx1),
  {TGuard, TCtx2} = translate_guard(Guard, TCtx1),
  {TBody, TCtx3} = translate_body(Meta, Body, TCtx2),
  TCtx4 = kapok_ctx:pop_scope(TCtx3),
  {{clause, ?line(Meta), [TPattern], TGuard, TBody}, TCtx4}.

%% fn
translate_fn(Meta, Exprs, Ctx) when is_list(Exprs) ->
  {Clauses, TCtx} = translate_fn_exprs(Exprs, Ctx),
  {{'fun', ?line(Meta), {clauses, Clauses}}, TCtx}.

translate_fn(Meta, Name, Arity, Ctx) when is_atom(Name), is_number(Arity) ->
  {{'fun', ?line(Meta), {function, Name, Arity}}, Ctx};
translate_fn(Meta, Name, Exprs, Ctx) when is_atom(Name), is_list(Exprs) ->
  {Clauses, TCtx} = translate_fn_exprs(Exprs, Ctx),
  {{'named_fun', ?line(Meta), Name, Clauses}, TCtx}.

translate_fn(Meta, Module, Name, Arity, Ctx)
    when is_atom(Module), is_atom(Name), is_number(Arity) ->
  {TModule, TCtx} = translate(Module, Ctx),
  {TName, TCtx1} = translate(Name, TCtx),
  {TArity, TCtx2} = translate(Arity, TCtx1),
  {{'fun', ?line(Meta), {function, TModule, TName, TArity}}, TCtx2};
translate_fn(Meta, Args, Guard, Body, Ctx)
    when is_tuple(Args), (is_list(Guard) orelse is_tuple(Guard)), is_list(Body) ->
  {Clause, TCtx} = translate_fn_clause(Meta, Args, Guard, Body, Ctx),
  {{'fun', ?line(Meta), {clauses, [Clause]}}, TCtx}.
translate_fn(Meta, Name, Args, Guard, Body, Ctx)
    when is_atom(Name), is_tuple(Args), (is_list(Guard) andalso is_tuple(Guard)), is_list(Body) ->
  {Clause, TCtx} = translate_fn_clause(Meta, Args, Guard, Body, Ctx),
  {{'named_fun', ?line(Meta), Name, [Clause]}, TCtx}.

translate_fn_exprs(Exprs, Ctx) when is_list(Exprs) ->
  check_fn_exprs(Exprs, Ctx),
  lists:mapfoldl(fun translate_fn_expr/2, Ctx, Exprs).

translate_fn_expr({list, Meta, [{literal_list, _, _} = Args,
                                {list, _, [{identifier, _, 'when'} | _]} = Guard | Body]},
                  Ctx) ->
  translate_fn_clause(Meta, Args, Guard, Body, Ctx);
translate_fn_expr({list, Meta, [{literal_list, _, _} = Args | Body]}, Ctx) ->
  translate_fn_clause(Meta, Args, [], Body, Ctx);
translate_fn_expr(Ast, Ctx) ->
  Error = {invalid_fn_expression, {Ast}},
  kapok_error:form_error(token_meta(Ast), ?m(Ctx, file), ?MODULE, Error).

translate_fn_clause(Meta, Args, Guard, Body, Ctx) ->
  Ctx1 = kapok_ctx:push_scope(Ctx),
  {TArgs, TCtx1} = translate_def_args(Args, Ctx1),
  {TGuard, TCtx2} = translate_guard(Guard, TCtx1),
  {TBody, TCtx3} = translate_body(Meta, Body, TCtx2),
  Clause = {clause, ?line(Meta), TArgs, TGuard, TBody},
  TCtx4 = kapok_ctx:pop_scope(TCtx3),
  {Clause, TCtx4}.

check_fn_exprs(Exprs, Ctx) when is_list(Exprs) ->
  lists:foldl(fun check_fn_expr_arity/2, {0, Ctx}, Exprs).
check_fn_expr_arity({list, Meta, [{literal_list, _, List} | _]}, {Last, Ctx}) ->
  Arity = length(List),
  case Last of
    0 ->
      {Arity, Ctx};
    Arity ->
      {Arity, Ctx};
    _ ->
      Error = {fn_clause_arity_mismatch, {Arity, Last}},
      kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, Error)
  end.

%% send
translate_send(Meta, Pid, Message, Ctx) ->
  {TPid, TCtx} = translate(Pid, Ctx),
  {TMessage, TCtx1} = translate(Message, TCtx),
  {{op, ?line(Meta), '!', TPid, TMessage}, TCtx1}.

%% receive
%% translate receive
translate_receive(Meta, [], Ctx) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {empty_receive_expr});
translate_receive(Meta, Clauses, Ctx) ->
  {Result, TCtx} = translate_receive(Meta, Clauses, [], Ctx),
  case Result of
    {TClauses, TExpr, TBody} when is_list(TClauses) ->
      {{'receive', ?line(Meta), TClauses, TExpr, TBody}, TCtx};
    TClauses when is_list(TClauses) ->
      {{'receive', ?line(Meta), TClauses}, TCtx}
  end.

translate_receive(_Meta, [], Acc, Ctx) ->
  {lists:reverse(Acc), Ctx};
translate_receive(Meta, [H], Acc, Ctx) ->
  case H of
    {list, Meta1, [{identifier, _, 'after'}, Expr | Body]} ->
      {TExpr, TCtx} = translate(Expr, Ctx),
      {TBody, TCtx1} = translate_body(Meta1, Body, TCtx),
      {{lists:reverse(Acc), TExpr, TBody}, TCtx1};
    _ ->
      {TClause, TCtx} = translate_case_clause(H, Ctx),
      translate_receive(Meta, [], [TClause | Acc], TCtx)
  end;
translate_receive(Meta, [H|T], Acc, Ctx) ->
  case H of
    {list, Meta1, [{identifier, _, 'after'}, _Expr | _Body]} ->
      kapok_error:form_error(Meta1, ?m(Ctx, file), ?MODULE, {after_clause_not_the_last});
    _ ->
      {TClause, TCtx} = translate_case_clause(H, Ctx),
      translate_receive(Meta, T, [TClause | Acc], TCtx)
  end.

%% try catch after block
translate_try(Meta, [], Ctx) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {empty_try_expr});
translate_try(Meta, [Expr|Left], Ctx) ->
  {TExpr, TCtx} = translate(Expr, Ctx),
  {CatchClauses, AfterBody, TCtx1} = translate_try_catch_after(Meta, Left, TCtx),
  %% notice that case clauses are always empty
  {{'try', ?line(Meta), [TExpr], [], CatchClauses, AfterBody}, TCtx1}.

translate_try_catch_after(Meta, [], Ctx) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {try_without_catch_or_after});
translate_try_catch_after(_Meta, [{list, Meta1, [{identifier, _, 'catch'} | CatchClauses]}], Ctx) ->
  {TCatchClauses, TCtx} = translate_catch_clauses(Meta1, CatchClauses, Ctx),
  {TCatchClauses, [], TCtx};
translate_try_catch_after(_Meta, [{list, Meta1, [{identifier, _, 'after'} | Body]}], Ctx) ->
  {TBody, TCtx} = translate_body(Meta1, Body, Ctx),
  {[], TBody, TCtx};
translate_try_catch_after(_Meta,
                          [{list, Meta1, [{identifier, _, 'catch'} | CatchClauses]},
                           {list, Meta2, [{identifier, _, 'after'} | Body]}],
                          Ctx)  ->
  {TCatchClauses, TCtx} = translate_catch_clauses(Meta1, CatchClauses, Ctx),
  {TBody, TCtx1} = translate_body(Meta2, Body, TCtx),
  {TCatchClauses, TBody, TCtx1};
translate_try_catch_after(Meta, Exprs, Ctx) ->
  Error = {invalid_catch_after_clause, {Exprs}},
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, Error).

translate_catch_clauses(Meta, [], Ctx) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {no_catch_clause});
translate_catch_clauses(_Meta, Clauses, Ctx) ->
  lists:mapfoldl(fun translate_catch_clause/2, Ctx, Clauses).

translate_catch_clause({list, Meta, []}, Ctx) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {empty_catch_clause});
translate_catch_clause({list, Meta, [E]}, Ctx) ->
  Error = {missing_catch_clause_body, {E}},
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, Error);
translate_catch_clause({list, Meta, [E, {list, _, [{identifier, _, 'when'} | _]} = Guard | Body]},
                       Ctx) ->
  translate_catch_clause(Meta, E, Guard, Body, Ctx);
translate_catch_clause({list, Meta, [E | B]}, Ctx) ->
  translate_catch_clause(Meta, E, [], B, Ctx);
translate_catch_clause({C, Meta, _} = Ast, Ctx) when C /= list ->
  Error = {invalid_catch_clause, {Ast}},
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, Error).

translate_catch_clause(Meta, Exception, Guard, Body, Ctx) ->
  Ctx1 = kapok_ctx:push_scope(Ctx),
  {TException, TCtx1} = translate_exception(Exception, Ctx1),
  {TGuard, TCtx2} = translate_guard(Guard, TCtx1),
  {TBody, TCtx3} = translate_body(Meta, Body, TCtx2),
  TCtx4 = kapok_ctx:pop_scope(TCtx3),
  {{clause, ?line(Meta), [TException], TGuard, TBody}, TCtx4}.

translate_exception({list, Meta, [{Category, _, Atom} = Type, Pattern]}, Ctx)
    when Category == keyword; Category == atom ->
  case lists:any(fun(X) -> Atom == X end, ['throw', 'exit', 'error']) of
    false ->
      Error = {invalid_exception_type, {Atom}},
      kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, Error);
    true ->
      ok
  end,
  {TType, TCtx} = translate(Type, Ctx),
  {TPattern, TCtx1} = translate_def_arg(Pattern, TCtx),
  Line = ?line(Meta),
  {{tuple, Line, [TType, TPattern, {var, Line, '_'}]}, TCtx1};
translate_exception(Pattern, Ctx) ->
  {TPattern, TCtx} = translate_def_arg(Pattern, Ctx),
  Line = ?line(token_meta(Pattern)),
  {{tuple, Line, [{keyword, Line, 'throw'}, TPattern, {var, Line, '_'}]}, TCtx}.

%% Error

format_error({let_odd_forms, {Form}}) ->
  io_lib:format("let requires an even number of forms in binding, unpaired form: ~p~n", [Form]);
format_error({empty_case_clause}) ->
  io_lib:format("case clause is empty");
format_error({missing_case_clause_body}) ->
  io_lib:format("case clause body is missing");
format_error({invalid_case_clause, {Ast}}) ->
  io_lib:format("invalid case clause ~w", [Ast]);
format_error({invalid_fn_expression, {Expr}}) ->
  io_lib:format("invalid fn expression ~w", [Expr]);
format_error({fn_clause_arity_mismatch, {Arity, Last}}) ->
  io_lib:format("fn clause arity ~B mismatch with last clause arity ~B~n", [Arity, Last]);
format_error({empty_receive_expr}) ->
  io_lib:format("empty receive expression", []);
format_error({after_clause_not_the_last}) ->
  io_lib:format("after clause should be the last expression of receive or try catch", []);
format_error({empty_try_expr}) ->
  io_lib:format("empty try expression", []);
format_error({try_without_catch_or_after}) ->
  io_lib:format("try expression without catch or after clause", []);
format_error({invalid_catch_after_clause, {Exprs}}) ->
  io_lib:format("invalid catch and after clause in try: ~w", [Exprs]);
format_error({empty_catch_clause}) ->
  io_lib:format("catch clause is empty", []);
format_error({missing_catch_clause_body}) ->
  io_lib:format("catch clause body is missing", []);
format_error({invalid_catch_clause, {Ast}}) ->
  io_lib:format("invalid catch clause: ~w", [Ast]);
format_error({invalid_exception_type, {Type}}) ->
  io_lib:format("invalid exception type: ~p", [Type]).
