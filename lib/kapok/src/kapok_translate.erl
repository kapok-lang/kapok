%% Translate Kapok AST to Erlang Abstract Format.
-module(kapok_translate).
-export([translate/2,
         translate_args/2,
         format_error/1]).
-include("kapok.hrl").

%% literals

%% number

%% integer
translate({number, Meta, Number}, Env) when is_integer(Number) ->
  {{integer, ?line(Meta), Number}, Env};
%% float
translate({number, Meta, Number}, Env) when is_float(Number) ->
  {{float, ?line(Meta), Number}, Env};

%% Operators
translate({Op, Meta, Number}, Env) when ?is_op(Op) ->
  {Erl, TEnv} = translate(Number, Env),
  {{op, ?line(Meta), Op, Erl}, TEnv};

%% atom
translate({atom, Meta, Atom}, Env) ->
  {{atom, ?line(Meta), Atom}, Env};

%% Identifiers
translate({identifier, Meta, Id}, #{context := Context} = Env) ->
  %% search env to check whether identifier is a variable
  NewEnv = case Context of
             match_vars -> kapok_env:add_var(Meta, Env, Id);
             _ -> kapok_env:check_var(Meta, Env, Id)
           end,
  {{var, ?line(Meta), Id}, NewEnv};

%% binary string
translate({binary_string, _Meta, Binary}, Env) ->
  translate(Binary, Env);

%% list string
translate({list_string, Meta, Binary}, Env) ->
  {{string, ?line(Meta), binary_to_list(Binary)}, Env};

%% Containers

%% bitstring
translate({bitstring, Meta, Arg}, Env) ->
  kapok_bitstring:translate(Meta, Arg, Env);

%% map
translate({map, Meta, Arg}, Env) ->
  kapok_map:translate(Meta, Arg, Env);

%% set
translate({set, Meta, Arg}, Env) ->
  kapok_set:translate(Meta, Arg, Env);

%% tuple
translate({tuple, Meta, Arg}, Env) ->
  {TArg, TEnv} = translate(Arg, Env),
  {{tuple, ?line(Meta), TArg}, TEnv};

%% list
translate({literal_list, _Meta, List}, Env) ->
  translate_list(List, Env);

translate({cons_list, Meta, {Head, Tail}}, Env) ->
  {THead, TEnv} = case Head of
                     [E] -> translate(E, Env);
                     _ -> translate(Head, Env)
                   end,
  {TTail, TEnv1} = translate(Tail, TEnv),
  {{cons, ?line(Meta), THead, TTail}, TEnv1};

%% special forms
%% let
translate({list, Meta, [{identifier, _, 'let'}, {C, _, Args} | Body]}, Env) when ?is_list(C) ->
  Env1 = kapok_env:push_scope(Env),
  {TArgs, TEnv1} = translate_let_args(Args, Env1),
  {TBody, TEnv2} = translate(Body, TEnv1),
  BodyBlock = to_block(0, TBody),
  {to_block(Meta, TArgs ++ [BodyBlock]), kapok_env:pop_scope(TEnv2)};

%% match
translate({list, Meta, [{identifier, _, '='}, Arg1, Arg2 | Left]}, Env) ->
  {TArg1, TEnv} = translate_match_pattern(Arg1, Env),
  {TArg2, TEnv1} = translate(Arg2, TEnv),
  Result = {match, ?line(Meta), TArg1, TArg2},
  case Left of
    [] -> {Result, TEnv1};
    _ -> translate_match(Meta, TArg2, Left, [Result], TEnv1)
  end;

%% do
translate({list, Meta, [{identifier, _, 'do'} | Exprs]}, Env) ->
  {TExprs, TEnv} = translate(Exprs, Env),
  {to_block(Meta, TExprs), TEnv};

%% case
translate({list, Meta, [{identifier, _, 'case'}, Expr, Clause | Left]}, Env) ->
  {TExpr, TEnv} = translate(Expr, Env),
  {TClause, TEnv1} = translate_case_clause(Clause, TEnv),
  {TLeft, TEnv2} = lists:mapfoldl(fun translate_case_clause/2, TEnv1, Left),
  {{'case', ?line(Meta), TExpr, [TClause | TLeft]}, TEnv2};

%% fn
translate({list, Meta, [{identifier, _, 'fn'}, {C, _, _} = Args | Body]}, Env) when ?is_list(C) ->
  translate_fn(Meta, Args, Body, Env);

%% Erlang specified forms

%% behaviour
translate({list, Meta, [{identifier, _, Form}, {Category, _, Id}]}, Env)
    when ?is_behaviour(Form), ?is_id(Category) ->
  translate_attribute(Meta, Form, Id, Env);

%% compile
translate({list, Meta, [{identifier, _, Form}, {Category, _, _} = Options]}, Env)
    when ?is_compile(Form), ?is_list(Category) ->
  {TOptions, TEnv} = kapok_compiler:ast(Options, Env),
  translate_attribute(Meta, Form, TOptions, TEnv);

%% file
translate({list, Meta, [{identifier, _, Form}, {C1, _, Binary}, {C2, _, Number}]}, Env)
    when ?is_file(Form), ?is_string(C1), ?is_number(C2) ->
  translate_attribute(Meta, Form, {Binary, Number}, Env);

%% wild attribute
translate({list, Meta, [{identifier, _, Form}, {C1, _, Attribute}, {C2, _, Value}]}, Env)
    when ?is_attribute(Form), ?is_id(C1), ?is_id(C2) ->
  translate_attribute(Meta, Attribute, Value, Env);

%% Local call
translate({list, Meta, [{Category, _, Id} | Args]}, #{scope := Scope} = Env) when ?is_id(Category) ->
  Arity = length(Args),
  FunArity = {Id, Arity},
  Vars = maps:get(vars, Scope),
  case orddict:find(Id, Vars) of
    {ok, _Var} ->
      %% local variable
      translate_local_call(Meta, Id, Args, Env);
    error ->
      case kapok_dispatch:find_local(FunArity, Env) of
        {F, A, P} ->
          translate_local_call(Meta, F, A, P, Arity, Args, Env);
        false ->
          %% check whether it's in imported functions/macros
          io:format("to find local function ~p~n", [{Id, Args}]),
          {R, Env1} = kapok_dispatch:find_local_function(Meta, FunArity, Env),
          case R of
            {M, F, A, P} -> translate_remote_call(Meta, M, F, A, P, Arity, Args, Env1);
            _ -> kapok_error:compile_error(Meta, ?m(Env1, file), "unknown local call: ~s", [Id])
          end
      end
  end;

%%  Remote call
translate({list, Meta, [{dot, _, {Prefix, Suffix}} | Args]}, Env) ->
  Arity = length(Args),
  FunArity = {Suffix, Arity},
  Namespace = ?m(Env, namespace),
  case Prefix of
    Namespace ->
      %% call to local module
      case kapok_dispatch:find_export(FunArity, Env) of
        {F1, A1, P1} -> translate_remote_call(Meta, Suffix, F1, A1, P1, Arity, Args, Env);
        _ -> kapok_error:compile_error(Meta, ?m(Env, file), "unknown remote call: ~s:~s", [Prefix, Suffix])
      end;
    _ ->
      {R, Env1} = kapok_dispatch:find_remote_function(Meta, Prefix, FunArity, Env),
      case R of
        {M2, F2, A2, P2} -> translate_remote_call(Meta, M2, F2, A2, P2, Arity, Args, Env1);
        _ -> translate_remote_call(Meta, Prefix, Suffix, Args, Env1)
      end
  end;

translate({list, Meta, [F | Args]}, Env) ->
  translate_local_call(Meta, F, Args, Env);
translate({list, _Meta, Args}, Env) ->
  translate_list(Args, [], Env);

%% a list of ast
translate(List, Env) when is_list(List) ->
  lists:mapfoldl(fun translate/2, Env, List);

translate(Other, Env) ->
  {to_abstract_format(Other), Env}.

%% Converts specified code to erlang abstract format

to_abstract_format(Tree) when is_tuple(Tree) ->
  {tuple, 0, [to_abstract_format(X) || X <- tuple_to_list(Tree)]};
to_abstract_format([]) ->
  {nil, 0};
to_abstract_format(<<>>) ->
  {bin, 0, []};
to_abstract_format(Tree) when is_list(Tree) ->
  to_abstract_format_cons_1(Tree, []);
to_abstract_format(Tree) when is_atom(Tree) ->
  {atom, 0, Tree};
to_abstract_format(Tree) when is_integer(Tree) ->
  {integer, 0, Tree};
to_abstract_format(Tree) when is_float(Tree) ->
  {float, 0, Tree};
to_abstract_format(Tree) when is_binary(Tree) ->
  %% Note that our binaries are utf-8 encoded and we are converting
  %% to a list using binary_to_list. The reason for this is that Erlang
  %% considers a string in a binary to be encoded in latin1, so the bytes
  %% are not changed in any fashion.
  {bin, 0, [{bin_element, 0, {string, 0, binary_to_list(Tree)}, default, default}]};
to_abstract_format(Function) when is_function(Function) ->
  case (erlang:fun_info(Function, type) == {type, external}) andalso
    (erlang:fun_info(Function, env) == {env, []}) of
    true ->
      {module, Module} = erlang:fun_info(Function, module),
      {name, Name}     = erlang:fun_info(Function, name),
      {arity, Arity}   = erlang:fun_info(Function, arity),

      {'fun', 0, {function,
                  {atom, 0, Module},
                  {atom, 0, Name},
                  {integer, 0, Arity}}};
    false ->
      error(badarg)
  end;

to_abstract_format(Pid) when is_pid(Pid) ->
  abstract_format_remote_call(0, erlang, binary_to_term, [to_abstract_format(term_to_binary(Pid))]);
to_abstract_format(_Other) ->
  error(badarg).

to_abstract_format_cons_1([H|T], Acc) ->
  to_abstract_format_cons_1(T, [H|Acc]);
to_abstract_format_cons_1(Other, Acc) ->
  to_abstract_format_cons_2(Acc, to_abstract_format(Other)).

to_abstract_format_cons_2([H|T], Acc) ->
  to_abstract_format_cons_2(T, {cons, 0, to_abstract_format(H), Acc});
to_abstract_format_cons_2([], Acc) ->
  Acc.

abstract_format_remote_call(Line, Module, Function, Args) ->
  {call, Line, {remote, Line, {atom, Line, Module}, {atom, Line, Function}}, Args}.


%% Helpers
translate_list(L, Env) ->
  translate_list(L, [], Env).
translate_list([H|T], Acc, Env) ->
  {Erl, TEnv} = translate(H, Env),
  translate_list(T, [Erl|Acc], TEnv);
translate_list([], Acc, Env) ->
  {build_list(Acc, {nil, 0}), Env}.

build_list([H|T], Acc) ->
  build_list(T, {cons, 0, H, Acc});
build_list([], Acc) ->
  Acc.

%% translate let
translate_let_args(Args, Env) ->
  translate_let_args(Args, [], Env#{context => match_vars}).
translate_let_args([], Acc, Env) ->
  {lists:reverse(Acc), Env#{context => nil}};
translate_let_args([P1, P2 | T], Acc, Env) ->
  {TP1, TEnv} = translate(P1, Env),
  {TP2, TEnv1} = translate(P2, TEnv),
  translate_let_args(T, [{match, ?line(kapok_scanner:token_meta(P1)), TP1, TP2} | Acc], TEnv1);
translate_let_args([H], _Acc, Env) ->
  Error = {let_odd_forms, {H}},
  kapok_error:form_error(kapok_scanner:token_meta(H), ?m(Env, file), ?MODULE, Error).

to_block(Meta, Exprs) when is_list(Meta) ->
  to_block(?line(Meta), Exprs);
to_block(Line, Exprs) when is_integer(Line) ->
  {block, Line, Exprs}.


%% translate match

translate_match_pattern(P, #{context := Context} = Env) ->
  {TP, TEnv} = translate(P, Env#{context => match_vars}),
  {TP, TEnv#{context => Context}}.

translate_match(_Meta, _Last, [], Acc, Env) ->
  {lists:reverse(Acc), Env};
translate_match(Meta, Last, [H|T], Acc, Env) ->
  {TH, TEnv} = translate(H, Env),
  translate_match(Meta, TH, T, [{match, ?line(Meta), Last, TH} | Acc], TEnv).

%% special forms

translate_case_clause({list, Meta, []}, Env) ->
  Error = {empty_case_clause},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);
translate_case_clause({list, Meta, [P]}, Env) ->
  Error = {missing_case_clause_body, {P}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);
translate_case_clause({list, Meta, [P, {list, _, [{identifier, _, 'when'} | _]} = Guard | Body]}, Env) ->
  translate_case_clause(Meta, P, Guard, Body, Env);
translate_case_clause({list, Meta, [P | B]}, Env) ->
  translate_case_clause(Meta, P, [], B, Env);
translate_case_clause({C, Meta, _} = Ast, Env) when C /= list ->
  Error = {invalid_case_clause, {Ast}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error).

translate_case_clause(Meta, Pattern, Guard, Body, Env) ->
  {TPattern, TEnv} = translate_match_pattern(Pattern, Env),
  {TGuard, TEnv1} = translate_guard(Guard, TEnv),
  case Body of
    [] ->
      Error = {missing_case_clause_body},
      kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);
    _ ->
      ok
  end,
  {TBody, TEnv2} = translate(Body, TEnv1),
  {{clause, ?line(Meta), [TPattern], TGuard, TBody}, TEnv2}.

translate_guard([], Env) ->
  {[], Env};
translate_guard({list, Meta, [{identifier, _, 'when'} | Body]}, Env) ->
  case Body of
    [] ->
      Error = {missing_case_clause_guard},
      kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);
    [H] ->
      translate_guard(H, nil, Env);
    [H | T] ->
      Error = {too_many_guards, {H, T}},
      kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error)
  end.
translate_guard({list, Meta, [{identifier, _, 'and'} | Left]}, 'and', Env) ->
  Error = {invalid_nested_and_or_in_guard, {'and', 'and', Left}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);
translate_guard({list, Meta, [{identifier, _, 'and'} | Left]}, Parent, Env) ->
  case Left of
    [E1, E2 | Tail] ->
      {TE1, TEnv} = translate_guard(E1, 'and', Env),
      {TE2, TEnv1} = translate_guard(E2, 'and', TEnv),
      {TLeft, TEnv2} = lists:mapfoldl(fun (X, E) -> translate_guard(X, 'and', E) end,
                                      TEnv1,
                                      Tail),
      L = [TE1, TE2 | TLeft],
      case Parent of
        nil -> {[L], TEnv2};
        'or' -> {L, TEnv2}
      end;
    [Tail] ->
      Error1 = {not_enough_operand_in_guard, {'and', Tail}},
      kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error1)
  end;
translate_guard({list, _, [{identifier, _, 'or'} | Left]}, nil, Env) ->
  lists:mapfoldl(fun (X, E) -> translate_guard(X, 'or', E) end, Env, Left);
translate_guard({list, Meta, [{identifier, _, 'or'} | Left]}, Parent, Env) ->
  Error = {invalid_nested_and_or_in_guard, {Parent, 'or', Left}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);
translate_guard(Other, nil, Env) ->
  {TOther, TEnv} = translate(Other, Env),
  %% as only expression in 'or'
  {[[TOther]], TEnv};
translate_guard(Other, 'or', Env) ->
  {TOther, TEnv} = translate(Other, Env),
  %% actually one expression in 'or'
  {[TOther], TEnv};
translate_guard(Other, _Parent, Env) ->
  translate(Other, Env).

%% translate fn

translate_fn(Meta, Args, Body, Env) ->
  Env1 = kapok_env:push_scope(Env),
  {TArgs, TEnv1} = translate_args(Args, Env1),
  {TBody, TEnv2} = translate(Body, TEnv1),
  Clause = {clause, ?line(Meta), TArgs, [], TBody},
  TEnv3 = kapok_env:pop_scope(TEnv2),
  {{'fun', ?line(Meta), {clauses, [Clause]}}, TEnv3}.


%% translate attribute

translate_attribute(Meta, A, T, Env) ->
  {{attribute,?line(Meta), A, T}, Env}.

%% translate local call
translate_local_call(Meta, F, A, P, Arity, Args, Env) ->
  Args1 = kapok_dispatch:construct_new_args('translate', Arity, A, P, Args),
  translate_local_call(Meta, F, Args1, Env).
translate_local_call(Meta, F, Args, Env) ->
  {TF, TEnv} = translate(F, Env),
  {TArgs, TEnv1} = translate(Args, TEnv),
  {{call, ?line(Meta), TF, TArgs}, TEnv1}.

%% translate remote call
translate_remote_call(Meta, M, F, A, P, Arity, Args, Env) ->
  Args1 = kapok_dispatch:construct_new_args('translate', Arity, A, P, Args),
  translate_remote_call(Meta, M, F, Args1, Env).
translate_remote_call(Meta, M, F, Args, Env) ->
  {TM, _} = translate(M, Env),
  {TF, _} = translate(F, Env),
  {TArgs, TEnv} = translate_args(Args, Env),
  Line = ?line(Meta),
  {{call, Line, {remote, Line, TM, TF}, TArgs}, TEnv}.

%% Translate args
translate_arg(Arg, #{context := Context} = Env) ->
  {TArg, TEnv} = translate(Arg, Env#{context => match_vars}),
  {TArg, TEnv#{context => Context}}.

translate_args({Category, _, Args}, Env) when ?is_list(Category) ->
  {TArgs, TEnv} = translate_args(Args, Env),
  %% reset Env.context for normal list args
  {TArgs, TEnv};
translate_args({Category, _, {Head, Tail}}, Env) when ?is_cons_list(Category) ->
  {THead, TEnv} = translate_args(Head, Env),
  {TTail, TEnv1} = translate_arg(Tail, TEnv),
  %% reset Env.context for cons list args
  {THead ++ [TTail], TEnv1};
translate_args(Args, Env) when is_list(Args) ->
  {TArgs, Env1} = lists:mapfoldl(fun translate_arg/2, Env, Args),
  {TArgs, Env1}.

%% Error

format_error({let_odd_forms, {Form}}) ->
  io_lib:format("let requires an even number of forms in binding, unpaired form: ~p~n", [Form]);
format_error({empty_case_clause}) ->
  io_lib:format("case clause is empty");
format_error({too_many_guards, {First, Left}}) ->
  io_lib:format("too many arguments for when, please use and/or for guard tests or sequences: ~p, ~p", [First, Left]);
format_error({missing_case_clause_guard}) ->
  io_lib:format("missing case clause guard");
format_error({invalid_nested_and_or_in_guard, {Parent, Id, Left}}) ->
  io_lib:format("invalid nested and/or in guard, parent: ~p, current: ~p, args: ~p", [Parent, Id, Left]);
format_error({not_enough_operand_in_guard, {Op, Operand}}) ->
  io_lib:format("not enough operand for ~p, given: ~p", [Op, Operand]).

