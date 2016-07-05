%% a temporary module for refactoring macro
-module(kapok_expr).
-export([expand_macro/2]).
-import(kapok_scanner, [token_meta/1, token_text/1]).
-include("kapok.hrl").

expand_macro(List, Env) when is_list(List) ->
  lists:mapfoldl(fun expand_macro/2, Env, List);
expand_macro({list, _, [{identifier, _, Id} | _Args]} = Ast, Env) when ?is_def(Id) ->
  {Ast, Env};
expand_macro({list, Meta, [{identifier, _, Id} | Args]} = Ast, Env) ->
  Arity = length(Args),
  {R, Env1} = kapok_dispatch:find_local_macro(Meta, {Id, Arity}, Env),
  case R of
    {M, F, A, P} ->
      NewArgs = kapok_translate:construct_new_args('expand', Arity, A, P, Args),
      kapok_dispatch:expand_macro_named(Meta, M, F, A, NewArgs, Env1);
    false ->
      {Ast, Env}
  end;
expand_macro({list, Meta, [{dot, _, {M, F}} | Args]} = Ast, Env) ->
  Arity = length(Args),
  {R, Env1} = kapok_dispatch:find_remote_macro(Meta, M, {F, Arity}, Env),
  case R of
    {M, F, A, P} ->
      NewArgs = kapok_translate:construct_new_args('expand', Arity, A, P, Args),
      kapok_dispatch:expand_macro_named(Meta, M, F, A, NewArgs, Env1);
    false ->
      {Ast, Env1}
  end;
expand_macro(Ast, Env) ->
  {Ast, Env}.

