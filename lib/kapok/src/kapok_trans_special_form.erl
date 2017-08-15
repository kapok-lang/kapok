%% special form
-module(kapok_trans_special_form).
-export([translate_attribute/4,
         translate_let/4,
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
translate_attribute(Meta, A, T, Env) ->
  {{attribute,?line(Meta), A, T}, Env}.

%% let
translate_let(Meta, Args, Body, Env) ->
  Env1 = kapok_env:push_scope(Env),
  {TArgs, TEnv1} = translate_let_args(Args, Env1),
  {TBody, TEnv2} = translate_body(Meta, Body, TEnv1),
  BodyBlock = build_block(0, TBody),
  TEnv3 = kapok_env:pop_scope(TEnv2),
  {build_block(Meta, TArgs ++ [BodyBlock]), TEnv3}.

translate_let_pattern(Arg, #{context := Context} = Env) ->
  {TArg, TEnv} = translate(Arg, Env#{context => let_pattern}),
  {TArg, TEnv#{context => Context}}.

translate_let_args(Args, Env) ->
  translate_let_args(Args, [], Env).
translate_let_args([], Acc, Env) ->
  {lists:reverse(Acc), Env#{context => nil}};
translate_let_args([H], _Acc, Env) ->
  Error = {let_odd_forms, {H}},
  kapok_error:form_error(kapok_scanner:token_meta(H), ?m(Env, file), ?MODULE, Error);
translate_let_args([P1, P2 | T], Acc, Env) ->
  {TP1, TEnv} = translate_let_pattern(P1, Env),
  {TP2, TEnv1} = translate(P2, TEnv),
  translate_let_args(T, [{match, ?line(kapok_scanner:token_meta(P1)), TP1, TP2} | Acc], TEnv1).

%% do
translate_do(Meta, Exprs, Env) ->
  {TExprs, TEnv} = translate(Exprs, Env),
  {build_block(Meta, TExprs), TEnv}.

build_block(Meta, Exprs) when is_list(Meta) ->
  build_block(?line(Meta), Exprs);
build_block(Line, Exprs) when is_integer(Line) ->
  {block, Line, Exprs}.

%% case
translate_case(Meta, Expr, Clause, Left, Env) ->
  {TExpr, TEnv} = translate(Expr, Env),
  {TClause, TEnv1} = translate_case_clause(Clause, TEnv),
  {TLeft, TEnv2} = lists:mapfoldl(fun translate_case_clause/2, TEnv1, Left),
  {{'case', ?line(Meta), TExpr, [TClause | TLeft]}, TEnv2}.

translate_case_clause({list, Meta, []}, Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {empty_case_clause});
translate_case_clause({list, Meta, [_P]}, Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {missing_case_clause_body});
translate_case_clause({list, Meta, [P, {list, _, [{identifier, _, 'when'} | _]} = Guard | Body]},
                      Env) ->
  translate_case_clause(Meta, P, Guard, Body, Env);
translate_case_clause({list, Meta, [P | B]}, Env) ->
  translate_case_clause(Meta, P, [], B, Env);
translate_case_clause({C, Meta, _} = Ast, Env) when C /= list ->
  Error = {invalid_case_clause, {Ast}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error).

translate_case_clause(Meta, Pattern, Guard, Body, Env) ->
  Env1 = kapok_env:push_scope(Env),
  {TPattern, TEnv1} = translate_def_arg(Pattern, Env1),
  {TGuard, TEnv2} = translate_guard(Guard, TEnv1),
  {TBody, TEnv3} = translate_body(Meta, Body, TEnv2),
  TEnv4 = kapok_env:pop_scope(TEnv3),
  {{clause, ?line(Meta), [TPattern], TGuard, TBody}, TEnv4}.

%% fn
translate_fn(Meta, Exprs, Env) when is_list(Exprs) ->
  {Clauses, TEnv} = translate_fn_exprs(Exprs, Env),
  {{'fun', ?line(Meta), {clauses, Clauses}}, TEnv}.

translate_fn(Meta, Name, Arity, Env) when is_atom(Name), is_number(Arity) ->
  {{'fun', ?line(Meta), {function, Name, Arity}}, Env};
translate_fn(Meta, Name, Exprs, Env) when is_atom(Name), is_list(Exprs) ->
  {Clauses, TEnv} = translate_fn_exprs(Exprs, Env),
  {{'named_fun', ?line(Meta), Name, Clauses}, TEnv}.

translate_fn(Meta, Module, Name, Arity, Env)
    when is_atom(Module), is_atom(Name), is_number(Arity) ->
  {TModule, TEnv} = translate(Module, Env),
  {TName, TEnv1} = translate(Name, TEnv),
  {TArity, TEnv2} = translate(Arity, TEnv1),
  {{'fun', ?line(Meta), {function, TModule, TName, TArity}}, TEnv2};
translate_fn(Meta, Args, Guard, Body, Env)
    when is_tuple(Args), (is_list(Guard) orelse is_tuple(Guard)), is_list(Body) ->
  {Clause, TEnv} = translate_fn_clause(Meta, Args, Guard, Body, Env),
  {{'fun', ?line(Meta), {clauses, [Clause]}}, TEnv}.
translate_fn(Meta, Name, Args, Guard, Body, Env)
    when is_atom(Name), is_tuple(Args), (is_list(Guard) andalso is_tuple(Guard)), is_list(Body) ->
  {Clause, TEnv} = translate_fn_clause(Meta, Args, Guard, Body, Env),
  {{'named_fun', ?line(Meta), Name, [Clause]}, TEnv}.

translate_fn_exprs(Exprs, Env) when is_list(Exprs) ->
  check_fn_exprs(Exprs, Env),
  lists:mapfoldl(fun translate_fn_expr/2, Env, Exprs).

translate_fn_expr({list, Meta, [{literal_list, _, _} = Args,
                                {list, _, [{identifier, _, 'when'} | _]} = Guard | Body]},
                  Env) ->
  translate_fn_clause(Meta, Args, Guard, Body, Env);
translate_fn_expr({list, Meta, [{literal_list, _, _} = Args | Body]}, Env) ->
  translate_fn_clause(Meta, Args, [], Body, Env);
translate_fn_expr(Ast, Env) ->
  Error = {invalid_fn_expression, {Ast}},
  kapok_error:form_error(token_meta(Ast), ?m(Env, file), ?MODULE, Error).

translate_fn_clause(Meta, Args, Guard, Body, Env) ->
  Env1 = kapok_env:push_scope(Env),
  {TArgs, TEnv1} = translate_def_args(Args, Env1),
  {TGuard, TEnv2} = translate_guard(Guard, TEnv1),
  {TBody, TEnv3} = translate_body(Meta, Body, TEnv2),
  Clause = {clause, ?line(Meta), TArgs, TGuard, TBody},
  TEnv4 = kapok_env:pop_scope(TEnv3),
  {Clause, TEnv4}.

check_fn_exprs(Exprs, Env) when is_list(Exprs) ->
  lists:foldl(fun check_fn_expr_arity/2, {0, Env}, Exprs).
check_fn_expr_arity({list, Meta, [{literal_list, _, List} | _]}, {Last, Env}) ->
  Arity = length(List),
  case Last of
    0 ->
      {Arity, Env};
    Arity ->
      {Arity, Env};
    _ ->
      Error = {fn_clause_arity_mismatch, {Arity, Last}},
      kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error)
  end.

%% send
translate_send(Meta, Pid, Message, Env) ->
  {TPid, TEnv} = translate(Pid, Env),
  {TMessage, TEnv1} = translate(Message, TEnv),
  {{op, ?line(Meta), '!', TPid, TMessage}, TEnv1}.

%% receive
%% translate receive
translate_receive(Meta, [], Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {empty_receive_expr});
translate_receive(Meta, Clauses, Env) ->
  {Result, TEnv} = translate_receive(Meta, Clauses, [], Env),
  case Result of
    {TClauses, TExpr, TBody} when is_list(TClauses) ->
      {{'receive', ?line(Meta), TClauses, TExpr, TBody}, TEnv};
    TClauses when is_list(TClauses) ->
      {{'receive', ?line(Meta), TClauses}, TEnv}
  end.

translate_receive(_Meta, [], Acc, Env) ->
  {lists:reverse(Acc), Env};
translate_receive(Meta, [H], Acc, Env) ->
  case H of
    {list, Meta1, [{identifier, _, 'after'}, Expr | Body]} ->
      {TExpr, TEnv} = translate(Expr, Env),
      {TBody, TEnv1} = translate_body(Meta1, Body, TEnv),
      {{lists:reverse(Acc), TExpr, TBody}, TEnv1};
    _ ->
      {TClause, TEnv} = translate_case_clause(H, Env),
      translate_receive(Meta, [], [TClause | Acc], TEnv)
  end;
translate_receive(Meta, [H|T], Acc, Env) ->
  case H of
    {list, Meta1, [{identifier, _, 'after'}, _Expr | _Body]} ->
      kapok_error:form_error(Meta1, ?m(Env, file), ?MODULE, {after_clause_not_the_last});
    _ ->
      {TClause, TEnv} = translate_case_clause(H, Env),
      translate_receive(Meta, T, [TClause | Acc], TEnv)
  end.

%% try catch after block
translate_try(Meta, [], Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {empty_try_expr});
translate_try(Meta, [Expr|Left], Env) ->
  {TExpr, TEnv} = translate(Expr, Env),
  {CatchClauses, AfterBody, TEnv1} = translate_try_catch_after(Meta, Left, TEnv),
  %% notice that case clauses are always empty
  {{'try', ?line(Meta), [TExpr], [], CatchClauses, AfterBody}, TEnv1}.

translate_try_catch_after(Meta, [], Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {try_without_catch_or_after});
translate_try_catch_after(_Meta, [{list, Meta1, [{identifier, _, 'catch'} | CatchClauses]}], Env) ->
  {TCatchClauses, TEnv} = translate_catch_clauses(Meta1, CatchClauses, Env),
  {TCatchClauses, [], TEnv};
translate_try_catch_after(_Meta, [{list, Meta1, [{identifier, _, 'after'} | Body]}], Env) ->
  {TBody, TEnv} = translate_body(Meta1, Body, Env),
  {[], TBody, TEnv};
translate_try_catch_after(_Meta,
                          [{list, Meta1, [{identifier, _, 'catch'} | CatchClauses]},
                           {list, Meta2, [{identifier, _, 'after'} | Body]}],
                          Env)  ->
  {TCatchClauses, TEnv} = translate_catch_clauses(Meta1, CatchClauses, Env),
  {TBody, TEnv1} = translate_body(Meta2, Body, TEnv),
  {TCatchClauses, TBody, TEnv1};
translate_try_catch_after(Meta, Exprs, Env) ->
  Error = {invalid_catch_after_clause, {Exprs}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error).

translate_catch_clauses(Meta, [], Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {no_catch_clause});
translate_catch_clauses(_Meta, Clauses, Env) ->
  lists:mapfoldl(fun translate_catch_clause/2, Env, Clauses).

translate_catch_clause({list, Meta, []}, Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {empty_catch_clause});
translate_catch_clause({list, Meta, [E]}, Env) ->
  Error = {missing_catch_clause_body, {E}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);
translate_catch_clause({list, Meta, [E, {list, _, [{identifier, _, 'when'} | _]} = Guard | Body]},
                       Env) ->
  translate_catch_clause(Meta, E, Guard, Body, Env);
translate_catch_clause({list, Meta, [E | B]}, Env) ->
  translate_catch_clause(Meta, E, [], B, Env);
translate_catch_clause({C, Meta, _} = Ast, Env) when C /= list ->
  Error = {invalid_catch_clause, {Ast}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error).

translate_catch_clause(Meta, Exception, Guard, Body, Env) ->
  Env1 = kapok_env:push_scope(Env),
  {TException, TEnv1} = translate_exception(Exception, Env1),
  {TGuard, TEnv2} = translate_guard(Guard, TEnv1),
  {TBody, TEnv3} = translate_body(Meta, Body, TEnv2),
  TEnv4 = kapok_env:pop_scope(TEnv3),
  {{clause, ?line(Meta), [TException], TGuard, TBody}, TEnv4}.

translate_exception({list, Meta, [{Category, _, Atom} = Type, Pattern]}, Env)
    when Category == keyword; Category == atom ->
  case lists:any(fun(X) -> Atom == X end, ['throw', 'exit', 'error']) of
    false ->
      Error = {invalid_exception_type, {Atom}},
      kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);
    true ->
      ok
  end,
  {TType, TEnv} = translate(Type, Env),
  {TPattern, TEnv1} = translate_def_arg(Pattern, TEnv),
  Line = ?line(Meta),
  {{tuple, Line, [TType, TPattern, {var, Line, '_'}]}, TEnv1};
translate_exception(Pattern, Env) ->
  {TPattern, TEnv} = translate_def_arg(Pattern, Env),
  Line = ?line(token_meta(Pattern)),
  {{tuple, Line, [{keyword, Line, 'throw'}, TPattern, {var, Line, '_'}]}, TEnv}.

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
