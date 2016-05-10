%% translate defs to Erlang abstract format.
-module(kapok_defs).
-export([translate/3]).
-include("kapok.hrl").


%% Translation

translate(Meta, [{identifier, _, _}], Env) ->
  kapok_error:compile_error(Meta, ?m(Env, file), "no def");

translate(Meta, [{identifier, _, Keyword}, {identifier, _, Id}, {ListType, Meta1, Args} | Left],
          #{namespace := Namespace} = Env)
    when ?is_list_type(ListType), (Keyword =:= "def" orelse Keyword =:= "defp") ->
  %% only support single clause and no guards
  {TArgs, TEnv} = kapok_translate:translate(Args, Env),
  {TBody, TEnv1} = translate_body(Left, TEnv),
  Arity = length(TArgs),
  Name = list_to_atom(Id),
  case Keyword of
    "def" -> kapok_namespace:add_export(Namespace, {Name, Arity});
    _ -> ok
  end,
  {{function, ?line(Meta), Name, Arity, [{clause,?line(Meta1),TArgs,[],TBody}]},
   TEnv1}.

translate_body(Ast, Env) ->
  kapok_translate:translate(Ast, Env).
