%% translate defs to Erlang abstract format.
-module(kapok_defs).
-export([translate/3]).

-include("kapok.hrl").


%% Translation

translate(Meta, [{identifier, _, _}], Env) ->
  kapok_error:compile_error(Meta, maps:get(file, Env), "no def");

translate(Meta, [{identifier, _, "def"}, {identifier, _, Id}, {ListType, _, Args} | Left],
          #{namespace := Namespace} = Env) when ListType == list; ListType == list_literal ->
  {TArgs, TEnv} = kapok_translate:translate(Args, Env),
  {TClause, TEnv1} = translate_clause(Left, TEnv),
  Arity = length(TArgs),
  Name = list_to_atom(Id),
  kapok_namespace:add_export(Namespace, {Name, Arity}),
  Line = ?line(Meta),
  {{function, Line, Name, Arity, TClause}, TEnv1};

translate(Meta, [{identifier, _, "defp"}, {identifier, _, Name} | _Left], Env) ->
  Line = ?line(Meta),
  {{function, Line, list_to_atom(Name), 0, []}, Env}.


translate_clause(Ast, Env) ->
  %% TODO add impl
  {Ast, Env}.
