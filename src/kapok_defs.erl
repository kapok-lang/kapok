%% translate defs to Erlang abstract format.
-module(kapok_defs).
-export([translate/3]).

-include("kapok.hrl").


%% Translation

translate(Meta, [{identifier, _, _}], Env) ->
  kapok_error:compile_error(Meta, maps:get(file, Env), "no def");

translate(Meta, [{identifier, _, "def"}, {identifier, _, Id} | _Left], #{namespace := Namespace} = Env) ->
  Name = list_to_atom(Id),
  kapok_namespace:add_export(Namespace, {Name, 0}),
  Line = ?line(Meta),
  {{function, Line, Name, 0, []}, Env};

translate(Meta, [{identifier, _, "defp"}, {identifier, _, Name} | _Left], Env) ->
  Line = ?line(Meta),
  {{function, Line, list_to_atom(Name), 0, []}, Env}.

