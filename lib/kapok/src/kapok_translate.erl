%% Translate Kapok AST to Erlang Abstract Format.
-module(kapok_translate).
-export([translate/2,
         translate_args/2,
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
  NewEnv = case Context of
             match_vars -> kapok_env:add_var(Meta, Env, Id);
             _ -> kapok_env:check_var(Meta, Env, Id)
           end,
  {{var, ?line(Meta), Id}, NewEnv};

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
      case kapok_dispatch:find_export(FunArity, Env) of
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
  io_lib:format("let requires an even number of forms in binding, unpaired form: ~p~n", [Form]).

