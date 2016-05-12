%% translate defs to Erlang abstract format.
-module(kapok_defs).
-export([translate/3]).
-include("kapok.hrl").


%% Translation

translate(Meta, [{identifier, _, Keyword}, {identifier, _, _}, {ListType, _, _} | _T] = Args, Env)
    when ?is_list_type(ListType), (Keyword == 'defn' orelse Keyword == 'defn-') ->
  Export = case Keyword of
             'defn' -> true;
             'defn-' -> false
           end,
  do_translate(Meta, Args, Export, Env);

translate(Meta, [{identifier, _, 'defmacro'}, {identifier, _, _}, {ListType, _, _} | _T] = Args, Env)
    when ?is_list_type(ListType) ->
  do_translate(Meta, Args, true, Env);

translate(Meta, Args, Env) ->
  kapok_error:compile_error(Meta, ?m(Env, file), "invalid expression: ~p", [Args]).

do_translate(Meta, [{identifier, _, _}, {identifier, _, Id}, {_, Meta1, Args} | Body], Export,
             #{namespace := Namespace} = Env) ->
  %% only support single clause and no guards
  {TArgs, TEnv} = kapok_translate:translate(Args, Env),
  {TBody, TEnv1} = translate_body(Body, TEnv),
  Arity = length(TArgs),
  case Export of
    true -> kapok_namespace:add_export(Namespace, {Id, Arity});
    false -> ok
  end,
  {{function, ?line(Meta), Id, Arity, [{clause, ?line(Meta1), TArgs, [], TBody}]},
   TEnv1}.


translate_body(Ast, Env) ->
  kapok_translate:translate(Ast, Env).
