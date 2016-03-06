%%
-module(ceiba_expand).
-export([expand/2, expand_args/2, expand_arg/2]).
-import(ceiba_errors, [compile_error/3, compile_error/4]).

%% Local calls

expand({'list', Meta, List}=_Ast, Env) ->
  io:format("get list ns~n"),
  {EArgs, {_EC, _EV}} = expand_list(List, fun expand_arg/2, {Env, Env}, []),
  {{'list', Meta, EArgs}, Env};

%% Literals

expand(List, Env) when is_list(List) ->
  {EArgs, {_EC, EV}} = expand_list(List, fun expand/2, Env, []),
  {EArgs, EV};

expand(Ast, Env) ->
  io:format("default expand!~n"),
  %%[H|_T] = Ast,
  %%;;io:format("first: ~p~n", [H]),
  {Ast, Env}.

%% Helpers

expand_list([H|T], Fun, Acc, List) ->
  {EArg, EAcc} = Fun(H, Acc),
  expand_list(T, Fun, EAcc, [EArg|List]);
expand_list([], _Fun, Acc, List) ->
  {lists:reverse(List), Acc}.

expand_arg({number, _, _} = Arg, Acc) ->
  {Arg, Acc};
expand_arg({atom, _, _} = Arg, Acc) ->
  {Arg, Acc};
expand_arg(Arg, {Acc1, Acc2}) ->
  {EArg, _EAcc} = expand(Arg, Acc1),
  {EArg, {Acc1, Acc2}}.


expand_args(Ast, Env) ->
  {Ast, Env}.

