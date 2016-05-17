%% Translate Kapok AST to Erlang Abstract Format.
-module(kapok_translate).
-export([translate/2,
         translate_arg/3,
         translate_args/2,
         to_abstract_format/1]).
-include("kapok.hrl").

%% block

translate({'__block__', Meta, Args}, Env) ->
  {TArgs, TEnv} = translate_block(Args, Env),
  {{block, ?line(Meta), TArgs}, TEnv};

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
translate({identifier, Meta, Identifier}, Env) ->
  %% search env to check whether identifier is a variable
  io:format("!!!!!!! translate ident: ~p~n", [Identifier]),
  {{var, ?line(Meta), Identifier}, Env};

%% binary string
translate({binary_string, _Meta, Binary}, Env) ->
  translate(Binary, Env);

%% list string
translate({list_string, Meta, CharList}, Env) ->
  {{string, ?line(Meta), binary_to_list(CharList)}, Env};

%% Containers

%% bitstring
translate({bitstring, Meta, Args}, Env) ->
  kapok_bitstring:translate(Meta, Args, Env);

%% tuple
translate({tuple, Meta, Value}, Env) ->
  {{tuple, ?line(Meta), Value}, Env};

%% list
translate({literal_list, _Meta, List}, Env) ->
  translate_list(List, [], Env);

%% Local call

%% special forms
translate({list, Meta, [{identifier, _, 'ns'} | _] = Args}, Env) ->
  kapok_namespace:translate(Meta, Args, Env);

translate({list, Meta, [{identifier, _, Id} | _] = Args}, Env)
    when Id == 'defn';
         Id == 'defn-';
         Id == 'defmacro' ->
  kapok_def:translate(Meta, Args, Env);

translate({list, Meta, [{identifier, _, Id} | Args]},
          #{functions := Functions, function_aliases := Aliases} = Env) ->
  FunArity = {Id, length(Args)},
  %% check whether it's a local call
  case ordsets:is_element(FunArity, kapok_env:get_exports(Env)) of
    true ->
      {TF, TEnv} = translate(Id, Env),
      {TArgs, TEnv1} = translate_args(Args, TEnv),
      {{call, ?line(Meta), TF, TArgs}, TEnv1};
    false ->
      %% check whether it's in imported function list
      case orddict:find(FunArity, Functions) of
        {ok, {M, {F, _A}}} ->
          {TM, _} = translate(M, Env),
          {TF, _} = translate(F, Env),
          {TArgs, TEnv} = translate_args(Args, Env),
          Line = ?line(Meta),
          {{call, Line, {remote, Line, TM, TF}, TArgs}, TEnv};
        {ok, {F, _A}} ->
          {TF, TEnv} = translate(F, Env),
          {TArgs, TEnv1} = translate_args(Args, TEnv),
          {{call, ?line(Meta), TF, TArgs}, TEnv1};
        error ->
          %% check whether it's in function alias list
          case orddict:find(FunArity, Aliases) of
            {ok, Function} ->
              {ok, {M, {F, _A}}} = orddict:find(Function, Functions),
              {TM, _} = translate(M, Env),
              {TF, _} = translate(F, Env),
              {TArgs, TEnv} = translate_args(Args, Env),
              Line = ?line(Meta),
              {{call, Line, {remote, Line, TM, TF}, TArgs}, TEnv};
            error ->
              kapok_error:compile_error(Meta, ?m(Env, file), "invalid identifier: ~s", [Id])
          end
      end
  end;

%%  Remote call
translate({list, Meta, [{dot, _, {Module, F}} | Args]},
          #{namespace := Namespace, requires := Requires, module_aliases := ModuleAliases} = Env) ->
  M = case Module of
        Namespace ->
          Namespace;
        _ ->
          %% check whether this module is required or aliased
          case ordsets:is_element(Module, Requires) of
            true ->
              Module;
            false ->
              case orddict:find(Module, ModuleAliases) of
                {ok, Original} -> Original;
                error -> kapok_error:compile_error(Meta, ?m(Env, file), "invalid module: ~p", [Module])
              end
          end
      end,
  {TM, _} = translate(M, Env),
  {TF, _} = translate(F, Env),
  {TArgs, TEnv} = translate_args(Args, Env),
  Line = ?line(Meta),
  {{call, Line, {remote, Line, TM, TF}, TArgs}, TEnv};

translate({list, _Meta, Args}, Env) ->
  translate_list(Args, [], Env);

translate({quote, _, Arg}, Env) ->
  {to_abstract_format(Arg), Env};
  %% case Arg of
  %%   {identifier, _, _} -> translate(Arg, Env);
  %%   _ -> {to_abstract_format(Arg), Env}
  %% end;

%% a list of ast
translate(List, Env) when is_list(List) ->
  lists:mapfoldl(fun (X, E) -> translate(X, E) end,
                 Env,
                 List);

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

%% Translate blocks

translate_block(Args, Env) ->
  translate_block(Args, [], Env).
translate_block([], Acc, Env) ->
  {lists:reverse(Acc), Env};
translate_block([H|T], Acc, Env) ->
  {TH, TEnv} = translate(H, Env),
  {T, [TH|Acc], TEnv}.


