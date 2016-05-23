%% Translate Kapok AST to Erlang Abstract Format.
-module(kapok_translate).
-export([translate/2,
         translate_arg/3,
         translate_args/2,
         to_abstract_format/1,
         format_error/1]).
-include("kapok.hrl").

%% literals

%% number

%% integer
translate({number, Meta, Value}, Env) when is_integer(Value) ->
  {{integer, ?line(Meta), Value}, Env};
%% float
translate({number, Meta, Value}, Env) when is_float(Value) ->
  {{float, ?line(Meta), Value}, Env};

%% Operators
translate({Op, Meta, Number}, Env) when Op == '+', Op == '-' ->
  {Erl, TEnv} = translate(Number, Env),
  {{op, ?line(Meta), Op, Erl}, TEnv};

%% atom
translate({atom, Meta, Value}, Env) ->
  {{atom, ?line(Meta), Value}, Env};

%% Identifiers
translate({identifier, Meta, Id}, #{context := Context} = Env) ->
  %% search env to check whether identifier is a variable
  case Context of
    match_vars -> kapok_env:add_var(Meta, Env, Id);
    _ -> kapok_env:check_var(Meta, Env, Id)
  end,
  {{var, ?line(Meta), Id}, Env};

%% binary string
translate({binary_string, _Meta, Binary}, Env) ->
  translate(Binary, Env);

%% list string
translate({list_string, Meta, Arg}, Env) ->
  {{string, ?line(Meta), binary_to_list(Arg)}, Env};

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
  translate_list(List, [], Env);

translate({cons_list, Meta, {Head, Tail}}, Env1) ->
  {THead, TEnv1} = case Head of
                     [E] -> translate(E, Env1);
                     _ -> translate(Head, Env1)
                   end,
  {TTail, TEnv2} = translate(Tail, TEnv1),
  {{cons, ?line(Meta), THead, TTail}, TEnv2};

%% Local call

%% special forms
%% let
translate({list, Meta, [{identifier, _, 'let'}, {ListType, _, Args} | Body]}, Env1)
    when ?is_list_type(ListType) ->
  {TArgs, TEnv1} = translate_let_args(Args, Env1),
  {TBody, TEnv2} = translate(Body, TEnv1),
  io:format("after translate let args: ~p~n", [TArgs]),
  io:format("after translate let body: ~p~n", [TBody]),
  {to_block(Meta, TArgs ++ TBody), TEnv2};

%% match
translate({list, Meta, [{identifier, _, '='}, Arg1, Arg2 | Left]}, Env1) ->
  {TArg1, TEnv1} = translate(Arg1, Env1),
  {TArg2, TEnv2} = translate(Arg2, TEnv1),
  Result = {match, ?line(Meta), TArg1, TArg2},
  case Left of
    [] -> {Result, TEnv2};
    _ -> translate_match(Meta, TArg2, Left, [Result], TEnv2)
  end;

%% do
translate({list, Meta, [{identifier, _, 'do'} | Exprs]}, Env) ->
  {TExprs, TEnv} = translate(Exprs, Env),
  {to_block(Meta, TExprs), TEnv};

translate({list, Meta, [{Category, _, Id} | Args]}, #{functions := Functions} = Env)
    when Category == identifier; Category == atom ->
  FunArity = {Id, length(Args)},
  %% check whether it's a local call
  Namespace = ?m(Env, namespace),
  case ordsets:is_element(FunArity, kapok_namespace:namespace_exports(Namespace)) of
    true ->
      {TF, TEnv} = translate(Id, Env),
      {TArgs, TEnv1} = translate_args(Args, TEnv),
      {{call, ?line(Meta), TF, TArgs}, TEnv1};
    false ->
      %% check whether it's in imported function list
      D = orddict:fold(fun (Receiver, Imports, Acc) ->
                           case [{F, A} || {F, A} <- Imports, {F, A} == FunArity] of
                             [] -> Acc;
                             L -> orddict:store(Receiver, L, Acc)
                           end
                       end,
                       [],
                       Functions),
      case D of
        [] -> kapok_error:compile_error(Meta, ?m(Env, file), "invalid identifier: ~s", [Id]);
        [{M, [{F, _}]}] ->
          {TM, _} = translate(M, Env),
          {TF, _} = translate(F, Env),
          {TArgs, TEnv} = translate_args(Args, Env),
          Line = ?line(Meta),
          {{call, Line, {remote, Line, TM, TF}, TArgs}, TEnv};
        _ -> kapok_error:compile_error(Meta, ?m(Env, file), "too many match for identifier: ~s", [Id])
      end
  end;

%%  Remote call
translate({list, Meta, [{dot, _, {M, F}} | Args]} = Ast, Env) ->
  io:format("to translate ~p~n", [Ast]),
  {TM, _} = translate(M, Env),
  {TF, _} = translate(F, Env),
  {TArgs, TEnv} = translate_args(Args, Env),
  Line = ?line(Meta),
  {{call, Line, {remote, Line, TM, TF}, TArgs}, TEnv};

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
  {lists:reverse(Acc), Env};
translate_let_args([P1, P2 | T], Acc, Env) ->
  {TP1, TEnv} = translate(P1, Env),
  {TP2, TEnv} = translate(P2, Env),
  translate_let_args(T, [{match, ?line(kapok_scanner:token_meta(P1)), TP1, TP2} | Acc], Env);
translate_let_args([H], _Acc, Env) ->
  Error = {let_odd_forms, {H}},
  kapok_error:form_error(kapok_scanner:token_meta(H), ?m(Env, file), ?MODULE, Error).

to_block(Meta, Exprs) ->
  {block, ?line(Meta), Exprs}.

%% translate match

translate_match(_Meta, _Last, [], Acc, Env) ->
  {lists:reverse(Acc), Env};
translate_match(Meta, Last, [H|T], Acc, Env) ->
  {TH, TEnv} = translate(H, Env),
  translate_match(Meta, TH, T, [{match, ?line(Meta), Last, TH} | Acc], TEnv).


%% Translate args

translate_arg(Arg, Acc, Env)
    when is_number(Arg); is_atom(Arg); is_binary(Arg); is_pid(Arg); is_function(Arg) ->
  {TArg, _} = translate(Arg, Env),
  {TArg, Acc};
translate_arg(Arg, Acc, _Env) ->
  {TArg, TAcc} = translate(Arg, Acc),
  {TArg, TAcc}.

translate_args(Args, #{context := match} = Env) ->
  lists:mapfoldl(fun translate/2, Env, Args);
translate_args(Args, Env) ->
  lists:mapfoldl(fun (X, Acc) -> translate_arg(X, Acc, Env) end,
                 Env,
                 Args).

%% Error

format_error({let_odd_forms, {Form}}) ->
  io_lib:format("let requires an even number of forms in binding, unpaired form: ~p~n", [Form]).

