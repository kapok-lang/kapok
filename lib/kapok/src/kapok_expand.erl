%%
-module(kapok_expand).
-export([macroexpand/2,
         macroexpand_n/3,
         macroexpand_1/2]).
-import(kapok_scanner, [token_meta/1, token_text/1]).
-include("kapok.hrl").

macroexpand(List, Env) when is_list(List) ->
  lists:mapfoldl(fun macroexpand/2, Env, List);
macroexpand(Ast, Env) ->
  {EAst, EEnv, _} = macroexpand_1(Ast, Env),
  %% io:format("--- macroexpand_1 ---~n~p~n --- return ---~n~p~n------~n", [Ast, EAst]),
  {EAst, EEnv}.

macroexpand_n(Ast, Env, N) when N == 0 ->
  {Ast, Env};
macroexpand_n(Ast, Env, N) when N > 0 ->
  {EAst, EEnv, Expanded} = macroexpand_1(Ast, Env),
  case Expanded of
    true -> macroexpand_n(EAst, EEnv, N-1);
    false -> {EAst, EEnv}
  end.

%% a list of ast
macroexpand_1(List, Env) when is_list(List) ->
  macroexpand_list(List, Env);

%% list
macroexpand_1({list, Meta, [{identifier, _, Id} = Ident | T]}, Env) when ?is_def(Id) ->
  %% TODO move defs into kapok.core as predefined macros
  {ET, EEnv, Expanded} = macroexpand_1(T, Env),
  {{list, Meta, [Ident | ET]}, EEnv, Expanded};
macroexpand_1({list, Meta, [{identifier, _, Id} | T]} = Ast, Env) ->
  Arity = length(T),
  {R, Env1} = kapok_dispatch:find_local_macro(Meta, {Id, Arity}, Env),
  case R of
    {M, F, A, P} ->
      NewArgs = kapok_translate:construct_new_args('expand', Arity, A, P, T),
      {EAst, EEnv} = kapok_dispatch:expand_macro_named(Meta, M, F, A, NewArgs, Env1),
      {EAst, EEnv, true};
    false ->
      macroexpand_list(Ast, Env)
  end;
macroexpand_1({list, Meta, [{dot, _, {Module, Fun}} | T]} = Ast, Env) ->
  Arity = length(T),
  {R, Env1} = kapok_dispatch:find_remote_macro(Meta, Module, {Fun, Arity}, Env),
  case R of
    {M, F, A, P} ->
      NewArgs = kapok_translate:construct_new_args('expand', Arity, A, P, T),
      {EAst, EEnv} = kapok_dispatch:expand_macro_named(Meta, M, F, A, NewArgs, Env1),
      {EAst, EEnv, true};
    false ->
      macroexpand_list(Ast, Env1)
  end;
%% list and literal list
macroexpand_1({Category, _, _} = Ast, Env) when ?is_list(Category) ->
  macroexpand_list(Ast, Env);
%% cons_list
macroexpand_1({Category, Meta, {Head, Tail}}, Env) when ?is_cons_list(Category) ->
  {EHead, EEnv1, Expanded1} = macroexpand_1(Head, Env),
  {ETail, EEnv2, Expanded2} = macroexpand_1(Tail, EEnv1),
  {{Category, Meta, {EHead, ETail}}, EEnv2, Expanded1 orelse Expanded2};
%% non-list containers
macroexpand_1({Category, Meta, Args}, Env)
    when Category == 'bitstring', is_list(Args);
         Category == 'tuple';
         Category == 'map';
         Category == 'set' ->
  {EArgs, EEnv, Expanded} = macroexpand_1(Args, Env),
  {{Category, Meta, EArgs}, EEnv, Expanded};

%% macro special forms

%% quote, backquote, unquote, unquote_splicing
macroexpand_1({Category, Meta, Arg}, Env) when Category =:= quote;
                                               Category =:= backquote;
                                               Category =:= unquote;
                                               Category =:= unquote_splicing ->
  {EArg, EEnv, Expanded} = macroexpand_1(Arg, Env),
  {{Category, Meta, EArg}, EEnv, Expanded};

%% atom
macroexpand_1(Ast, Env) ->
  {Ast, Env, false}.


%% Helper functions

macroexpand_list({Category, Meta, List}, Env) when ?is_list(Category), is_list(List) ->
  {EList, EEnv, Expanded} = macroexpand_list(List, Env),
  {{Category, Meta, EList}, EEnv, Expanded};
macroexpand_list(List, Env) when is_list(List) ->
  {EList, {EEnv, Expanded}} = lists:mapfoldl(fun(Ast, {Env1, Expanded1}) ->
                                             {EAst, EEnv1, Expanded2} = macroexpand_1(Ast, Env1),
                                             {EAst, {EEnv1, Expanded2 orelse Expanded1}}
                                         end,
                                         {Env, false},
                                         List),
  {EList, EEnv, Expanded}.
