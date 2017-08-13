-module(kapok_macro).
-export(['__info__'/1,
         expand/2,
         expand_n/3,
         expand_1/2,
         append/2,
         'list*'/2]).
-import(kapok_scanner, [token_meta/1, token_text/1]).
-import(kapok_config, [get_compiler_opt/1]).
-include("kapok.hrl").

'__info__'(functions) ->
  [];
'__info__'(macros) ->
  [{append, 2, 'normal'},
   {'list*', 2, 'normal'}].

%% Expending macros.

expand(List, Env) when is_list(List) ->
  lists:mapfoldl(fun expand/2, Env, List);
expand(Ast, Env) ->
  {EAst, EEnv, Expanded} = expand_1(Ast, Env),
  case Expanded of
    true -> expand(EAst, EEnv);
    false -> {EAst, EEnv}
  end.

expand_n(Ast, Env, N) when N == 0 ->
  {Ast, Env};
expand_n(Ast, Env, N) when N > 0 ->
  {EAst, EEnv, Expanded} = expand_1(Ast, Env),
  case Expanded of
    true -> expand_n(EAst, EEnv, N-1);
    false -> {EAst, EEnv}
  end.

%% a list of ast
expand_1(List, Env) when is_list(List) ->
  expand_list(List, Env);

%% list
expand_1({list, Meta, [{identifier, _, Id} = Ident | T]}, Env) when ?is_def(Id) ->
  %% TODO move defs into kapok.core as predefined macros
  {ET, EEnv, Expanded} = expand_1(T, Env),
  {{list, Meta, [Ident | ET]}, EEnv, Expanded};
expand_1({list, Meta, [{identifier, _, Id} | T]} = Ast, Env) ->
  Arity = length(T),
  {R, Env1} = kapok_dispatch:find_local_macro(Meta, {Id, Arity}, Env),
  case R of
    {F, A, P} ->
      %% to call a previously defined macro in the namespace
      Namespace = ?m(Env, namespace),
      kapok_code:load_ns(Namespace, Env),
      {ok, Module} = kapok_code:get_module(Namespace),
      NewArgs = kapok_trans:construct_new_args('expand', Arity, A, P, T),
      {EAst, EEnv} = expand_macro_named(Meta, Module, F, A, NewArgs, Env),
      {EAst, EEnv, true};
    {M, F, A, P} ->
      %% to call a macro defined in another module
      NewArgs = kapok_trans:construct_new_args('expand', Arity, A, P, T),
      {EAst, EEnv} = expand_macro_named(Meta, M, F, A, NewArgs, Env1),
      {EAst, EEnv, true};
    false ->
      expand_list(Ast, Env)
  end;
expand_1({list, Meta, [{dot, _, {Module, Fun}} | T]} = Ast, Env) ->
  Arity = length(T),
  {R, Env1} = kapok_dispatch:find_remote_macro(Meta, Module, {Fun, Arity}, Env),
  case R of
    {M, F, A, P} ->
      NewArgs = kapok_trans:construct_new_args('expand', Arity, A, P, T),
      {EAst, EEnv} = expand_macro_named(Meta, M, F, A, NewArgs, Env1),
      {EAst, EEnv, true};
    false ->
      expand_list(Ast, Env1)
  end;
%% list and literal list
expand_1({Category, _, _} = Ast, Env) when ?is_list(Category) ->
  expand_list(Ast, Env);
%% cons_list
expand_1({Category, Meta, {Head, Tail}}, Env) when ?is_cons_list(Category) ->
  {EHead, EEnv1, Expanded1} = expand_1(Head, Env),
  {ETail, EEnv2, Expanded2} = expand_1(Tail, EEnv1),
  {{Category, Meta, {EHead, ETail}}, EEnv2, Expanded1 orelse Expanded2};
%% non-list containers
expand_1({Category, Meta, Args}, Env)
    when Category == 'bitstring', is_list(Args);
         Category == 'tuple';
         Category == 'map';
         Category == 'set' ->
  {EArgs, EEnv, Expanded} = expand_1(Args, Env),
  {{Category, Meta, EArgs}, EEnv, Expanded};

%% macro special forms

%% quote, backquote, unquote, unquote_splicing
expand_1({Category, Meta, Arg}, Env) when Category =:= quote;
                                          Category =:= backquote;
                                          Category =:= unquote;
                                          Category =:= unquote_splicing ->
  {EArg, EEnv, Expanded} = expand_1(Arg, Env),
  {{Category, Meta, EArg}, EEnv, Expanded};

%% atom
expand_1(Ast, Env) ->
  {Ast, Env, false}.

expand_list({Category, Meta, List}, Env) when ?is_list(Category), is_list(List) ->
  {EList, EEnv, Expanded} = expand_list(List, Env),
  {{Category, Meta, EList}, EEnv, Expanded};
expand_list(List, Env) when is_list(List) ->
  {EList, {EEnv, Expanded}} = lists:mapfoldl(fun(Ast, {Env1, Expanded1}) ->
                                                 {EAst, EEnv1, Expanded2} = expand_1(Ast, Env1),
                                                 {EAst, {EEnv1, Expanded2 orelse Expanded1}}
                                             end,
                                             {Env, false},
                                             List),
  {EList, EEnv, Expanded}.


%% macro expansion helper functions

expand_macro_named(Meta, Receiver, Name, Arity, Args, Env) ->
  case get_compiler_opt(debug) of
    true -> io:format("macro ~s:~s/~B args:~n~p~n", [Receiver, Name, Arity, Args]);
    false -> ok
  end,
  Fun = fun Receiver:Name/Arity,
  Result = expand_macro_fun(Meta, Fun, Receiver, Name, Args, Env),
  case get_compiler_opt(debug) of
    true -> io:format("macro ~s:~s/~B result:~n~p~n", [Receiver, Name, Arity, Result]);
    false -> ok
  end,
  {Result, Env}.

expand_macro_fun(Meta, Fun, Receiver, Name, Args, Env) ->
  Line = ?line(Meta),
  try
    apply(Fun, Args)
  catch
    Kind:Reason ->
      Arity = length(Args),
      MFA = {Receiver, Name, Arity},
      Info = [{Receiver, Name, Arity, [{file, "expand macro"}]}, caller(Line, Env)],
      erlang:raise(Kind, Reason, prune_stacktrace(erlang:get_stacktrace(), MFA, Info, nil))
  end.

caller(Line, #{namespace := Namespace} = Env) ->
  {Namespace, undefined, undefined, location(Line, Env)}.

location(Line, Env) ->
  [{file, kapok_utils:characters_to_list(?m(Env, file))},
   {line, Line}].

%% We've reached the invoked macro, skip it with the rest
prune_stacktrace([{M, F, A, _} | _], {M, F, A}, Info, _Env) ->
  Info;
%% We've reached the expand/dispatch internals, skip it with the rest
prune_stacktrace([{Mod, _, _, _} | _], _MFA, Info, _Env)
    when Mod == kapok_dispatch; Mod == kapok_macro ->
  Info;
prune_stacktrace([H|T], MFA, Info, Env) ->
  [H|prune_stacktrace(T, MFA, Info, Env)];
prune_stacktrace([], _MFA, Info, _Env) ->
  Info.


%% List building after evaluating macros.

append(Ast1, Ast2) ->
  Env = kapok_env:env_for_eval([{line, ?LINE}, {file, kapok_utils:to_binary(?FILE)}]),
  {EAst1, _} = expand(Ast1, Env),
  {EAst2, _} = expand(Ast2, Env),
  RAst = do_append(EAst1, EAst2),
  case get_compiler_opt(debug) of
    true -> io:format("--- call kapok_macro:append() ---~nAst1: ~p~nAst2: ~p~nresult: ~p~n===~n",
                      [Ast1, Ast2, RAst]);
    false -> ok
  end,
  RAst.

do_append({Category1, Meta1, List1}, {Category2, _, List2})
    when ?is_list(Category1), ?is_list(Category2), is_list(List1), is_list(List2) ->
  {Category2, Meta1, lists:append(List1, List2)};
do_append(List1, {Category2, Meta2, List2})
    when ?is_list(Category2), is_list(List1), is_list(List2) ->
  {Category2, Meta2, lists:append(List1, List2)};
do_append(Ast1, Ast2) ->
  kapok_error:compile_error(token_meta(Ast1),
                            <<"in macro:append()">>,
                            "invalid arguments, (~s, ~s)",
                            [token_text(Ast1), token_text(Ast2)]).

'list*'(Ast1, Ast2) ->
  Env = kapok_env:env_for_eval([{line, ?LINE}, {file, kapok_utils:to_binary(?FILE)}]),
  {EAst1, _} = expand(Ast1, Env),
  {EAst2, _} = expand(Ast2, Env),
  RAst = 'do_list*'(EAst1, EAst2),
  case get_compiler_opt(debug) of
    true -> io:format("--- call kapok_macro:list*() ---~nAst1: ~p~nAst2: ~p~nresult: ~p~n===~n",
                      [Ast1, Ast2, RAst]);
    false -> ok
  end,
  RAst.

'do_list*'({Category1, Meta1, List1}, {Category2, _Meta2, List2})
    when ?is_list(Category1), is_list(List1), ?is_list(Category2), is_list(List2) ->
  {Category1, Meta1, lists:append(List1, List2)};
'do_list*'(Ast1, Ast2) ->
  kapok_error:compile_error(token_meta(Ast1),
                            <<"in macro:list*()">>,
                            "invalid arguments: (~s, ~s)",
                            [token_text(Ast1), token_text(Ast2)]).
