%% translate def to Erlang abstract format.
-module(kapok_def).
-export([translate/3]).
-include("kapok.hrl").


%% Translation

translate(Meta, [{identifier, _, Keyword}, {identifier, _, _}, {ListType, _, _} | _T] = Args, Env)
    when ?is_list_type(ListType), (Keyword == 'defn' orelse Keyword == 'defn-') ->
  Export = case Keyword of
             'defn' -> true;
             'defn-' -> false
           end,
  do_translate(Meta, Args, {Export, 'fun'}, Env);

translate(Meta, [{identifier, _, 'defmacro'}, {identifier, _, _}, {ListType, _, _} | _T] = Args, Env)
    when ?is_list_type(ListType) ->
  do_translate(Meta, Args, {true, 'macro'}, Env);

translate(Meta, Args, Env) ->
  kapok_error:compile_error(Meta, ?m(Env, file), "invalid expression: ~p", [Args]).

do_translate(Meta, [{identifier, _, _}, {identifier, _, Id}, {_, Meta1, Args} | Body], {Export, Type}, Env) ->
  %% only support single clause and no guards
  Env1 = kapok_env:push_scope(Env),
  {TArgs, TEnv1} = translate_args(Args, Env1),
  io:format("after translate args, vars: ~p~n", [maps:get(vars, maps:get(scope, TEnv1))]),
  {TBody, TEnv2} = translate_body(Body, TEnv1),
  Arity = length(TArgs),
  TEnv3 = case Export of
    true ->
      case Type of
        'fun' -> kapok_env:add_export_function(Meta, Env, {Id, Arity});
        'macro' -> kapok_env:add_export_macro(Meta, Env, {Id, Arity})
      end;
    false ->
      TEnv2
  end,
  {{function, ?line(Meta), Id, Arity, [{clause, ?line(Meta1), TArgs, [], TBody}]},
   kapok_env:pop_scope(TEnv3)}.

translate_args(Args, Env) ->
  kapok_translate:translate(Args, Env).
  %% lists:mapfoldl(fun ({identifier, Meta, Id} = Ast, E) -> E1 = kapok_env:add_var(Meta, E, Id),
  %%                                                         kapok_translate:translate(Ast, E1);
  %%                    ({_, Meta, _} = Ast, E) -> kapok_error:compile_error(Meta, ?m(E, file), "invalid var: ~p", [Ast])
  %%                end,
  %%                Env,
  %%                Args).



translate_body(Ast, Env) ->
  kapok_translate:translate(Ast, Env).
