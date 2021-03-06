%% special form
-module(kapok_trans_special_form).
-export([translate_attribute/4,
         translate_let/4,
         translate_dot_let/4,
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
-import(kapok_scanner, [token_meta/1, token_symbol/1]).
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
  BodyBlock = build_block(Meta, TBody),
  TCtx3 = kapok_ctx:pop_scope(TCtx2),
  {build_block(Meta, TArgs ++ [BodyBlock]), TCtx3}.

%% dot let
translate_dot_let(Meta, Args, Tail, Ctx) ->
  Ctx1 = kapok_ctx:push_scope(Ctx),
  {TArgs, TCtx1, [Module, Fun]} = translate_dot_let_args(Args, Ctx1),
  Dot = {dot, Meta, {Module, Fun}},
  Body = [{list, Meta, [Dot | Tail]}],
  {TBody, TCtx2} = translate_body(Meta, Body, TCtx1),
  TCtx3 = kapok_ctx:pop_scope(TCtx2),
  {build_block(Meta, TArgs ++ TBody), TCtx3}.

translate_let_pattern(Arg, #{context := Context} = Ctx) ->
  {TArg, TCtx} = translate(Arg, Ctx#{context => let_pattern}),
  {TArg, TCtx#{context => Context}}.

translate_dot_let_pattern(Arg, #{context := Context} = Ctx) ->
  {TArg, TCtx} = translate(Arg, Ctx#{context => dot_let_pattern}),
  {TArg, TCtx#{context => Context}, token_symbol(TArg)}.

translate_let_args(Args, Ctx) ->
  translate_let_args(Args, [], Ctx).
translate_let_args([], Acc, Ctx) ->
  {lists:reverse(Acc), Ctx#{context => nil}};
translate_let_args([H], _Acc, Ctx) ->
  Error = {let_odd_forms, {H}},
  kapok_error:form_error(token_meta(H), ?m(Ctx, file), ?MODULE, Error);
translate_let_args([Pattern, Value | T], Acc, Ctx) ->
  %% Translate the value part first, and then the pattern part.
  %% Otherwise if there is a call of the pattern in the value part, it will
  %% cause a weird compile error saying "VAR_xxx is unbound", where `VAR_xxx`
  %% is the variable name of the translated pattern.
  {TValue, TCtx} = translate(Value, Ctx),
  {TPattern, TCtx1} = translate_let_pattern(Pattern, TCtx),
  translate_let_args(T, [{match, ?line(token_meta(Pattern)), TPattern, TValue} | Acc], TCtx1).

translate_dot_let_arg({C, Meta, Name}, Ctx) when ?is_id(C) ->
  case kapok_ctx:get_var(Meta, Ctx, Name) of
    {ok, _} ->
      {[], Ctx, {identifier, Meta, Name}};
    _ ->
      {[], Ctx, {atom, Meta, Name}}
  end;
translate_dot_let_arg({C, Meta, Name}, Ctx) when ?is_keyword_or_atom(C) ->
  {[], Ctx, {atom, Meta, Name}};
translate_dot_let_arg({_, Meta, _} = Dot, Ctx) ->
  {TDot, TCtx} = translate(Dot, Ctx),
  {TName, TCtx1, Name} = translate_dot_let_pattern({identifier, Meta, 'DOT'}, TCtx),
  Ast = {match, ?line(Meta), TName, TDot},
  Id = {identifier, Meta, Name},
  {[Ast], TCtx1, Id}.

translate_dot_let_args(Args, Ctx) ->
  {AstList, Ctx1, IdList} = lists:foldl(fun (Ast, {AstAcc, C, IdAcc}) ->
                                            {Ast1, C1, Id1} = translate_dot_let_arg(Ast, C),
                                            {Ast1 ++ AstAcc, C1, [Id1 | IdAcc]}
                                        end,
                                        {[], Ctx, []},
                                        Args),
  {lists:reverse(AstList), Ctx1, lists:reverse(IdList)}.

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
translate_case_clause({list, Meta, [P, {list, _, [{keyword_when, _, _} | _]} = Guard | Body]},
                      Ctx) ->
  translate_case_clause(Meta, P, Guard, Body, Ctx);
translate_case_clause({list, Meta, [P | B]}, Ctx) ->
  translate_case_clause(Meta, P, [], B, Ctx);
translate_case_clause({C, Meta, _} = Ast, Ctx) when C /= list ->
  Error = {invalid_case_clause, {Ast}},
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, Error).

translate_case_clause(Meta, Pattern, Guard, Body, Ctx) ->
  Ctx1 = kapok_ctx:push_scope(Ctx),
  {TPattern, TCtx1} = translate_case_pattern(Pattern, Ctx1),
  {TGuard, TCtx2} = translate_guard(Guard, TCtx1),
  {TBody, TCtx3} = translate_body(Meta, Body, TCtx2),
  TCtx4 = kapok_ctx:pop_scope(TCtx3),
  {{clause, ?line(Meta), [TPattern], TGuard, TBody}, TCtx4}.

translate_case_pattern(Arg, #{context := Context} = Ctx) ->
  {TArg, TCtx} = translate(Arg, Ctx#{context => case_pattern}),
  {TArg, TCtx#{context => Context}}.

%% fn
translate_fn(Meta, Exprs, Ctx) when is_list(Exprs) ->
  {Clauses, TCtx} = translate_fn_exprs(Exprs, Ctx),
  {{'fun', ?line(Meta), {clauses, Clauses}}, TCtx}.

translate_fn(Meta, Name, Arity, Ctx) when is_atom(Name), is_number(Arity) ->
  {{'fun', ?line(Meta), {function, Name, Arity}}, Ctx};
translate_fn(Meta, {identifier, _, Name} = Id, Exprs, Ctx) when is_list(Exprs) ->
  Ctx1 = kapok_ctx:push_scope(Ctx),
  {_, TCtx1} = translate_def_arg(Id, Ctx1),
  {Clauses, TCtx2} = translate_fn_exprs(Exprs, TCtx1),
  TCtx3 = kapok_ctx:pop_scope(TCtx2),
  {{'named_fun', ?line(Meta), Name, Clauses}, TCtx3}.

translate_fn(Meta, {C1, _, _} = Module, {C2, _, _} = Name, {C3, _, _} = Arity, Ctx)
    when ?is_local_id(C1), ?is_local_id(C2), ?is_number(C3) ->
  {TModule, TCtx} = translate(Module, Ctx),
  {TName, TCtx1} = translate(Name, TCtx),
  {TArity, TCtx2} = translate(Arity, TCtx1),
  {{'fun', ?line(Meta), {function, TModule, TName, TArity}}, TCtx2};
translate_fn(Meta, Args, Guard, Body, Ctx)
    when is_tuple(Args), (is_list(Guard) orelse is_tuple(Guard)), is_list(Body) ->
  {Clause, TCtx} = translate_fn_clause(Meta, Args, Guard, Body, Ctx),
  {{'fun', ?line(Meta), {clauses, [Clause]}}, TCtx}.
translate_fn(Meta, {identifier, _, Name} = Id, Args, Guard, Body, Ctx)
    when is_tuple(Args), (is_list(Guard) orelse is_tuple(Guard)), is_list(Body) ->
  Ctx1 = kapok_ctx:push_scope(Ctx),
  {_, TCtx1} = translate_def_arg(Id, Ctx1),
  {Clause, TCtx2} = translate_fn_clause(Meta, Args, Guard, Body, TCtx1),
  TCtx3 = kapok_ctx:pop_scope(TCtx2),
  {{'named_fun', ?line(Meta), Name, [Clause]}, TCtx3}.

translate_fn_exprs(Exprs, Ctx) when is_list(Exprs) ->
  check_fn_exprs(Exprs, Ctx),
  lists:mapfoldl(fun translate_fn_expr/2, Ctx, Exprs).

translate_fn_expr({list, Meta, [{literal_list, _, _} = Args,
                                {list, _, [{keyword_when, _, _} | _]} = Guard | Body]},
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
  {CaseClauses, CatchClauses, AfterBody, TCtx1} = translate_try_body(Meta, Left, TCtx),
  %% notice that case clauses are always empty
  {{'try', ?line(Meta), [TExpr], CaseClauses, CatchClauses, AfterBody}, TCtx1}.

translate_try_body(Meta, [], Ctx) ->
  kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, {try_without_body});
translate_try_body(Meta, [{list, _, [{list, _, _} | _] = Clauses} | Left], Ctx) ->
  {TClauses, TCtx} = lists:mapfoldl(fun translate_case_clause/2, Ctx, Clauses),
  {TCatchClauses, AfterBody, TCtx1} = translate_try_catch_after(Meta, Left, TCtx),
  {TClauses, TCatchClauses, AfterBody, TCtx1};
translate_try_body(Meta, Left, Ctx) ->
  {TCatchClauses, AfterBody, TCtx1} = translate_try_catch_after(Meta, Left, Ctx),
  {[], TCatchClauses, AfterBody, TCtx1}.

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
translate_catch_clause({list, Meta, [E, {list, _, [{keyword_when, _, _} | _]} = Guard | Body]},
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

translate_exception({list, Meta, [{Category, _, Atom} = Kind, Pattern]}, Ctx)
    when Category == keyword; Category == atom ->
  case lists:any(fun(X) -> Atom == X end, ['throw', 'exit', 'error']) of
    false ->
      Error = {invalid_exception_type, {Atom}},
      kapok_error:form_error(Meta, ?m(Ctx, file), ?MODULE, Error);
    true ->
      ok
  end,
  {TKind, TCtx} = translate(Kind, Ctx),
  {TPattern, TCtx1} = translate_case_pattern(Pattern, TCtx),
  Line = ?line(Meta),
  {{tuple, Line, [TKind, TPattern, {var, Line, '_'}]}, TCtx1};
translate_exception({list, Meta, [Kind, Pattern]}, Ctx) ->
  {TKind, TCtx} = translate_case_pattern(Kind, Ctx),
  {TPattern, TCtx1} = translate_case_pattern(Pattern, TCtx),
  Line = ?line(Meta),
  {{tuple, Line, [TKind, TPattern, {var, Line, '_'}]}, TCtx1};
translate_exception(Pattern, Ctx) ->
  {TPattern, TCtx} = translate_case_pattern(Pattern, Ctx),
  Line = ?line(token_meta(Pattern)),
  {{tuple, Line, [{atom, Line, 'throw'}, TPattern, {var, Line, '_'}]}, TCtx}.

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
format_error({try_without_body}) ->
  io_lib:format("try expression without body", []);
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
