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
  [].

default_functions() ->
  [].

default_macros() ->
  [].

find_local(Meta, Name, Args, Env) ->
  FA = {Name, length(Args)},
  case find_dispatch(Meta, FA, Env) of
    {function, R} -> R;
    {macro, R} -> R;
    _ -> false
  end.

%% dispatch

dispatch_local(Meta, Name, Args, Env, Callback) ->
  Arity = length(Args),
  case expand_local(Meta, {Name, Arity}, Args, Env) of
    {ok, Receiver, {EAst, EEnv}} ->
      io:format("before expand ast, env: ~p~n", [EEnv]),
      expand_ast(Meta, Receiver, Name, Arity, EAst, EEnv);
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
  Dispatch = find_dispatch(Meta, FA, Env),
  Macro = macro_for(Name, Arity),
  case Dispatch of
    {macro, {Receiver, {NewName, NewArity, ParaType}}} ->
      NewArgs = construct_new_args(Arity, NewArity, ParaType, Args),
      {ok, Receiver, expand_macro_named(Meta, Receiver, NewName, NewArity, NewArgs, Env)};
    {function, {Receiver, {NewName, NewArity, ParaType}}} ->
      NewArgs = construct_new_args(Arity, NewArity, ParaType, Args),
      {ok, Receiver, NewName, NewArgs};
    _ when Macro /= nil ->
      {Receiver, {NewName, NewArity, ParaType}} = Macro,
      NewArgs = construct_new_args(Arity, NewArity, ParaType, Args),
      {ok, Receiver, expand_macro_named(Meta, Receiver, NewName, NewArity, NewArgs, Env)};
    _ ->
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
  ProperArity = Arity+1,
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
    {[], [E]} -> {macro, E};
    {[E], []} -> {function, E};
    {[], []} -> false;
    _ ->
      {Name, Arity} = FA,
      [First | _] = FunMatch,
      [Second | _] = MacroMatch,
      Error = {ambiguous_call, {First, Second, Name, Arity}},
      kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error)
  end.

find_dispatch({Fun, Arity} = FA, List) when is_list(List) ->
  lists:foldl(fun ({Module, Imports}, Acc0) ->
                  R = ordsets:fold(
                          fun ({F, A} = E, Acc) when is_number(A) andalso E == FA -> [{F, A, normal} | Acc];
                              ({Alias, {F, A}}, Acc) when {Alias, A} == FA -> [{F, A, normal} | Acc];
                              ({F, A, normal} = E, Acc) when {F, A} == FA -> [E | Acc];
                              ({F, A, rest} = E, Acc) when (Fun == F) andalso (Arity >= A) -> [E | Acc];
                              (_, Acc) -> Acc
                          end,
                          [],
                          Imports),
                  case R of
                    [] -> Acc0;
                    _ -> orddict:store(Module, R, Acc0)
                  end
              end,
              [],
              List).

construct_new_args(Arity, NewArity, ParaType, Args) ->
  case (ParaType == rest) andalso (Arity >= NewArity) of
    true ->
      {NormalParas, RestPara} = lists:split(NewArity-1, Args),
      NormalParas ++ [RestPara];
    false ->
      Args
  end.

macro_for(Name, Arity) ->
  %% check for predifined macro
  Fun = list_to_atom("MACRO-" ++ atom_to_list(Name)),
  Macros = [],
  Result = lists:filter(fun ({F, A, normal}) -> {Name, Arity} == {F, A};
                            ({F, A, rest}) ->
                            (Fun == F) andalso (Arity >= A)
                        end,
                        Macros),
  case Result of
    [] -> nil;
    [E] -> {kapok_bootstrap, E}
  end.

get_optional_macros(Receiver) ->
  case code:ensure_loaded(Receiver) of
    {module, Receiver} ->
      try
        Receiver:'__info__'(macros)
      catch
        error:undef -> []
      end;
    {error, _} -> []
  end.


%% ERROR HANDLING

format_error({invalid_expression, {Ast}}) ->
  io_lib:format("invalid expression ~p", [Ast]);
format_error({ambiguous_call, {Mod1, Mod2, Name, Arity}}) ->
  io_lib:format("function ~ts/~B imported from both ~ts and ~ts, call in ambiguous",
               [Name, Arity, Mod1, Mod2]).


