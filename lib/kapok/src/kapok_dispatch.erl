%% Helper module for dispatching names(module/function/macro/var) and their references.
-module(kapok_dispatch).
-export([default_requires/0,
         default_functions/0,
         default_macros/0,
         find_local_macro/3,
         find_remote_macro/4,
         find_export/2,
         find_local_function/3,
         find_remote_function/4,
         find_fa/2,
         construct_new_args/5,
         expand_macro_named/6,
         format_error/1
        ]).
-include("kapok.hrl").

default_requires() ->
  [].

default_functions() ->
  [].

default_macros() ->
  [].

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

%% find local/remote macro/function

find_local_macro(Meta, FunArity, Env) ->
  io:format("call find_local_macro() FunArity: ~p~n", [FunArity]),
  {D, Env1} = find_dispatch(Meta, FunArity, Env),
  R = case D of
        {macro, {M, F, A, P}} -> {M, F, A, P};
        {function, _} -> false;
        false -> false
      end,
  {R, Env1}.

find_remote_macro(Meta, Module, FunArity, Env) ->
  Requires = ?m(Env, requires),
  Uses = ?m(Env, uses),
  case orddict:find(Module, Requires) of
    {ok, M1} ->
      case orddict:find(Module, Uses) of
        {ok, _} ->
          {D, Env} = find_dispatch(Meta, Module, FunArity, Env),
          R = case D of
                {macro, {M, F, A, P}} -> {M, F, A, P};
                {function, _} -> false;
                false -> false
              end,
          {R, Env};
        error ->
          {D, Env} = find_optional_dispatch(Meta, M1, FunArity, Env),
          R = case D of
                {macro, {M, F, A, P}} -> {M, F, A, P};
                {function, _} -> false;
                false -> false
              end,
          {R, Env}
      end;
    error ->
      {false, Env}
  end.

find_export(FunArity, Env) ->
  Namespace = maps:get(namespace, Env),
  Exports = kapok_namespace:namespace_exports(Namespace),
  case find_fa(FunArity, Exports) of
    [{F, A, P}] -> {F, A, P};
    [] -> false
  end.

find_local_function(Meta, FunArity, Env) ->
  {D, Env1} = find_dispatch(Meta, FunArity, Env),
  R = case D of
        {Tag, MFAP} when Tag == macro; Tag == function -> MFAP;
        false -> false
      end,
  {R, Env1}.

find_remote_function(Meta, Module, FunArity, #{requires := Requires} = Env) ->
  Requires = ?m(Env, requires),
  Uses = ?m(Env, uses),
  case orddict:find(Module, Requires) of
    {ok, M1} ->
      case orddict:find(M1, Uses) of
        {ok, _} ->
          {D, Env1} = find_dispatch(Meta, Module, FunArity, Env),
          R = case D of
                {Tag, MFAP} when Tag == macro; Tag == function -> MFAP;
                false -> false
              end,
          {R, Env1};
        error ->
          {D, Env1} = find_optional_dispatch(Meta, M1, FunArity, Env),
          R = case D of
                {Tag, MFAP} when Tag == macro; Tag == function -> MFAP;
                false -> false
              end,
          {R, Env1}
        end;
    error ->
      false
  end.

find_optional_dispatch(Meta, Module, FunArity, Env) ->
  FunList = get_optional_functions(Module),
  MacroList = get_optional_macros(Module),
  find_dispatch_fa(Meta, Module, FunArity, FunList, MacroList, Env).

find_dispatch(Meta, Module, FunArity, Env) ->
  Env1 = ensure_uses_imported(Env),
  FunList = case orddict:find(Module, ?m(Env1, functions)) of
              {ok, L1} -> L1;
              error -> []
            end,
  MacroList = case orddict:find(Module, ?m(Env1, macros)) of
                {ok, L2} -> L2;
                error -> []
              end,
  find_dispatch_fa(Meta, Module, FunArity, FunList, MacroList, Env1).

find_dispatch_fa(Meta, Module, {Fun, Arity} = FunArity, FunList, MacroList, Env) ->
  FunMatch = find_fa(FunArity, FunList),
  MacroMatch = find_fa(FunArity, MacroList),
  case {FunMatch, MacroMatch} of
    {[], [Match]} ->
      {F, A, P} = Match,
      {{macro, {Module, kapok_utils:macro_name(F), A, P}}, Env};
    {[Match], []} ->
      {F, A, P} = Match,
      {{function, {Module, F, A, P}}, Env};
    {[], []} ->
      {false, Env};
    _ ->
      [First, Second | _T] = FunMatch ++ MacroMatch,
      Error = {ambiguous_call, {Module, Fun, Arity, First, Second}},
      kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error)
  end.

find_dispatch(Meta, {Fun, Arity} = FunArity, Env) ->
  Env1 = ensure_uses_imported(Env),
  FunMatch = find_mfa(FunArity, ?m(Env1, functions)),
  MacroMatch = find_mfa(FunArity, ?m(Env1, macros)),
  io:format("find FA: ~s, ~B, macros: ~p, macromatch: ~p~n", [Fun, Arity, ?m(Env1, macros), MacroMatch]),
  case {FunMatch, MacroMatch} of
    {[], [Match]} ->
      {M, [{F, A, P}]} = Match,
      {{macro, {M, kapok_utils:macro_name(F), A, P}}, Env1};
    {[Match], []} ->
      {M, [{F, A, P}]} = Match,
      {{function, {M, F, A, P}}, Env1};
    {[], []} ->
      {false, Env1};
    _ ->
      [First, Second | _T] = FunMatch ++ MacroMatch,
      Error = {ambiguous_call, {Fun, Arity, First, Second}},
      kapok_error:form_error(Meta, ?m(Env1, file), ?MODULE, Error)
  end.


find_mfa(FunArity, List) when is_list(List) ->
  lists:foldl(fun({Module, Imports}, Acc) ->
                  case find_fa(FunArity, Imports) of
                    [] -> Acc;
                    R -> orddict:store(Module, R, Acc)
                  end
              end,
              [],
              List).

find_fa({Fun, Arity} = FunArity, FAList) when is_list(FAList) ->
  ordsets:fold(
      fun({F, A} = FA, Acc) when is_number(A) andalso FA == FunArity -> [{F, A, 'normal'} | Acc];
         ({Alias, {F, A, 'normal'}}, Acc) when {Alias, A} == FunArity -> [{F, A, 'normal'} | Acc];
         ({Alias, {F, A, 'rest'}}, Acc) when (Alias == Fun) andalso (A =< Arity) -> [{F, A, 'rest'} | Acc];
         ({F, A, 'normal'} = FAP, Acc) when {F, A} == FunArity -> [FAP | Acc];
         ({F, A, 'rest'} = FAP, Acc) when (F == Fun) andalso (A =< Arity) -> [FAP | Acc];
         (_, Acc) -> Acc
      end,
      [],
      FAList).

construct_new_args(Context, Arity, NewArity, ParaType, Args) ->
  case (ParaType == rest) andalso (Arity >= NewArity) of
    true ->
      {NormalParas, RestPara} = lists:split(NewArity-1, Args),
      case Context of
        'expand' -> NormalParas ++ [RestPara];
        'translate' -> NormalParas ++ [{list, [], RestPara}]
      end;
    false ->
      Args
  end.

ensure_uses_imported(#{uses := Uses} = Env) ->
  lists:foldl(fun({Module, Args}, E) ->
                  {ok, Meta} = orddict:find(meta, Args),
                  case module_is_imported(Module, E) of
                    true -> E;
                    false -> import_module(Meta, Module, Args, E)
                  end
              end,
              Env,
              Uses).

module_is_imported(Module, #{functions := Functions, macros := Macros}) ->
  orddict:is_key(Module, Functions) orelse orddict:is_key(Module, Macros).

import_module(Meta, Module, Args, Env) ->
  {Functions, Macros} = get_exports(Meta, Module, Args, Env),
  Env1 = case Functions of
           [] -> Env;
           _ -> kapok_env:add_function(Meta, Env, Module, Functions)
         end,
  Env2 = case Macros of
           [] -> Env1;
           _ -> kapok_env:add_macro(Meta, Env1, Module, Macros)
         end,
  io:format("after import module: ~p, Functions: ~p, Macros: ~p~n", [Module, ?m(Env2, functions), ?m(Env2, macros)]),
  Env2.

get_exports(Meta, Module, Args, Env) ->
  Functions = get_functions(Meta, Module, Env),
  Macros = get_macros(Meta, Module, Env),
  {filter_exports(Functions, Args),
   filter_exports(Macros, Args)}.

ensure_loaded(Meta, Module, Env) ->
  case code:ensure_loaded(Module) of
    {module, Module} ->
      ok;
    {error, What} ->
      kapok_error:compile_error(Meta, ?m(Env, file), "fail to load module: ~p due to load error: ~p", [Module, What])
  end.

get_optional_functions(Module) ->
  case code:ensure_loaded(Module) of
    {module, Module} ->
      try
        L = Module:'__info__'(funs),
        ordsets:from_list(L)
      catch
        error:undef ->
          try
            L1 = Module:module_info(exports),
            ordsets:from_list(lists:map(fun({F, A}) -> {F, A, 'normal'} end, L1))
          catch
            error:undef -> []
          end
      end;
    {error, _} -> []
  end.

get_functions(Meta, Module, Env) ->
  ensure_loaded(Meta, Module, Env),
  try
    L = Module:'__info__'(funs),
    ordsets:from_list(L)
  catch
    error:undef ->
      try
        L1 = Module:module_info(exports),
        ordsets:from_list(lists:map(fun({F, A}) -> {F, A, 'normal'} end, L1))
      catch
        error:undef ->
          kapok_error:compile_error(Meta, ?m(Env, file), "fail to get exports for unloaded module: ~p", [Module])
      end
  end.

get_optional_macros(Module) ->
  case code:ensure_loaded(Module) of
    {module, Module} ->
      try
        L = Module:'__info__'(macros),
        ordsets:from_list(L)
      catch
        error:undef -> []
      end;
    {error, _} -> []
  end.

get_macros(Meta, Module, Env) ->
  ensure_loaded(Meta, Module, Env),
  try
    io:format("+++++++ to get_macros ~n"),
    L = Module:'__info__'(macros),
    io:format("+++++++ get macros ~p~n", [L]),
    ordsets:from_list(L)
 catch
   error:undef -> []
  end.

filter_exports(Exports, Args) ->
  ordsets:from_list(lists:foldl(fun filter_exports_by/2, Exports, Args)).
filter_exports_by({'only', Includes}, Exports) ->
  lists:filter(fun(FAP) ->
                   Match = lists:filter(fun(E) -> match_fa(E, FAP) end, Includes),
                   case Match of
                     [] -> false;
                     _ -> true
                   end
               end,
               Exports);
filter_exports_by({'exclude', Excludes}, Exports) ->
  lists:filter(fun(FAP)  ->
                   Match = lists:filter(fun(E) -> match_fa(E, FAP) end, Excludes),
                   case Match of
                     [] -> true;
                     _ -> false
                   end
               end,
               Exports);
filter_exports_by({'rename', Renames}, Exports) ->
  lists:foldl(fun({Fun, Arity, ParaType} = FAP, Acc) ->
                  New = lists:foldl(fun({Alias, {F, A}}, Acc0) ->
                                        case (F == Fun) andalso (A == Arity) of
                                          true -> [{Alias, {Fun, Arity, ParaType}} | Acc0];
                                          _ -> Acc0
                                        end;
                                       ({Alias, F}, Acc0) ->
                                        case (F == Fun) of
                                          true -> [{Alias, {Fun, Arity, ParaType}} | Acc0];
                                          _ -> Acc0
                                        end
                                    end,
                                    [],
                                    Renames),
                  case New of
                    [] -> [FAP | Acc];
                    _ -> New ++ Acc
                  end
              end,
              [],
              Exports);
filter_exports_by(_, Exports) ->
  Exports.

match_fa({F, A}, {Fun, Arity, _ParaType}) ->
  (F == Fun) andalso (A == Arity);
match_fa(Name, {Fun, _Arity, _ParaType}) when is_atom(Name) ->
  Name == Fun.

%% ERROR HANDLING

format_error({invalid_expression, {Ast}}) ->
  io_lib:format("invalid expression ~p", [Ast]);
format_error({ambiguous_call, {M, F, A, FAP1, FAP2}}) ->
  io_lib:format("find function ~ts:~ts/~B duplicates in ~p and ~p", [M, F, A, FAP1, FAP2]);
format_error({ambiguous_call, {F, A, FAP1, FAP2}}) ->
  io_lib:format("function ~ts/~B imported from both ~ts and ~ts, call in ambiguous", [F, A, FAP1, FAP2]).


