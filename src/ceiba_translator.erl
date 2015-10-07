%% Translate Ceiba AST to Erlang Abstract Format.
-module(ceiba_translator).
-export([translate/1]).

translate({list, Meta, [Head|Tail]}) ->
    {list, Meta, [Head|Tail]}.


