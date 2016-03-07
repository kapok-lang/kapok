%%
-module(kapok_expand).
-export([expand/2]).

%% function/macro calls

expand({'list', Meta, List}=_Ast, Env) ->
  {EArgs, NewEnv} = expand_list(List, fun expand/2, Env),
  {{'list', Meta, EArgs}, NewEnv};

%% special forms

expand({quote, _, _} = Ast, Env) ->
  %% don't expand quote expression
  {Ast, Env};

expand({backquote, Meta, List}, Env) ->
  %% TODO add impl
  {{backquote, Meta, List}, Env};

expand({unquote, Meta, List}, Env) ->
  %% TODO add impl
  {{unquote, Meta, List}, Env};

expand({unquote_splicing, Meta, List}, Env) ->
  %% TODO add impl
  {{unquote_splicing, Meta, List}, Env};

%% Literals

expand(List, Env) when is_list(List) ->
  {EArgs, NewEnv} = expand_list(List, fun expand/2, Env),
  {EArgs, NewEnv};

expand(Ast, Env) ->
  {Ast, Env}.

%% Helpers

expand_list(List, Fun, Env) ->
  expand_list(List, Fun, Env, []).
expand_list([H|T], Fun, Env, Acc) ->
  {EArg, NewEnv} = Fun(H, Env),
  expand_list(T, Fun, NewEnv, [EArg|Acc]);
expand_list([], _Fun, Env, Acc) ->
  {lists:reverse(Acc), Env}.

