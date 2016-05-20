%% Helper module for dispatching names(module/function/macro/var) and their references.
-module(kapok_dispatch).
-export([default_requires/0,
         default_functions/0,
         default_macros/0,
         find_local/4,
         dispatch_local/5,
         dispatch_remote/6,
         format_error/1
        ]).
-include("kapok.hrl").

default_requires() ->
  ['Kapok.Core'].

default_functions() ->
  lists:map(fun ({Name, Arity}) -> {{Name, Arity}, {kapok_bootstrap, {Name, Arity}}} end,
            kapok_bootstrap:'__info__'(functions)).

default_macros() ->
  lists:map(fun ({Name, Arity}) ->
                "MACRO-" ++ F = atom_to_list(Name),
                {{F, Arity}, {kapok_bootstrap, {Name, Arity}}}
            end,
            kapok_bootstrap:'__info__'(macros)).

find_local(Meta, Name, Args, Env) ->
  FA = {Name, length(Args)},
  case find_dispatch(Meta, FA, Env) of
    {function, Receiver} ->
      Receiver;
    {macro, Receiver} ->
      Receiver;
    _ ->
      false
  end.

%% dispatch

dispatch_local(Meta, Name, Args, Env, Callback) ->
  Arity = length(Args),
  case expand_local(Meta, {Name, Arity}, Args, Env) of
    {ok, Receiver, Ast} ->
      expand_ast(Meta, Receiver, Name, Arity, Ast, Env);
    {ok, Receiver, NewName, NewArgs} ->
      kapok_expand:expand({list, Meta, [{dot, Meta, [Receiver, NewName]} | NewArgs]}, Env);
    error ->
      Callback()
  end.

dispatch_remote(Meta, Receiver, Name, Args, Env, Callback) ->
  Arity = length(Args),
  case expand_remote(Meta, Receiver, {Name, Arity}, Args, Env) of
    {ok, Receiver, Ast} -> expand_ast(Meta, Receiver, Name, Arity, Ast, Env);
    error -> Callback(Receiver, Name, Args)
  end.

%% expand

expand_local(Meta, {Name, Arity} = FA, Args, Env) ->
  case find_dispatch(Meta, FA, Env) of
    {macro, Receiver} ->
      {ok, Receiver, expand_macro_named(Meta, Receiver, Name, Arity, Args, Env)};
    {function, Receiver} ->
      {ok, Receiver, Name, Args};
    error ->
      error
  end.

expand_remote(Meta, Receiver, {Name, Arity} = FA, Args, Env) ->
  case find_dispatch(Meta, FA, Env) of
    {macro, Receiver} ->
      {ok, Receiver, expand_macro_named(Meta, Receiver, Name, Arity, Args, Env)};
    _ ->
      error
  end.

%% expand helpers

expand_macro_fun(Meta, Fun, _Receiver, _Name, Args, Env) ->
  Line = ?line(Meta),
  CallerArg = {Line, Env},
  try
    apply(Fun, [CallerArg | Args])
  catch
    Kind:Reason ->
      erlang:raise(Kind, Reason, erlang:get_stacktrace())
  end.

expand_macro_named(Meta, Receiver, Name, Arity, Args, Env) ->
  ProperArity = Arity + 1,
  Fun = fun Receiver:Name/ProperArity,
  expand_macro_fun(Meta, Fun, Receiver, Name, Args, Env).

expand_ast(_Meta, _Receiver, _Name, _Arity, Ast, Env) ->
  try
    kapok_expand:expand(Ast, Env)
  catch
    Kind:Reason ->
      erlang:raise(Kind, Reason, erlang:get_stacktrace())
  end.

%% Helpers

find_dispatch(Meta, FA, #{functions := Functions,
                          macros := Macros} = Env) ->
  FunMatch = find_dispatch(FA, Functions),
  MacroMatch = find_dispatch(FA, Macros),
  case {FunMatch, MacroMatch} of
    {[], [Receiver]} -> {macro, Receiver};
    {[Receiver], []} -> {function, Receiver};
    {[], []} -> false;
    _ ->
      {Name, Arity} = FA,
      [First | _] = FunMatch,
      [Second | _] = MacroMatch,
      Error = {ambiguous_call, {First, Second, Name, Arity}},
      kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error)
  end.

find_dispatch(FA, List) when is_list(List) ->
  [Receiver || {Receiver, Set} <- List, orddict:is_key(FA, Set)].

%% ERROR HANDLING

format_error({ambiguous_call, {Mod1, Mod2, Name, Arity}}) ->
  io_lib:format("function ~ts/~B imported from both ~ts and ~ts, call in ambiguous",
               [Name, Arity, Mod1, Mod2]).


