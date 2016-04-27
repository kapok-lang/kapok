%% core module
-module(kapok_core).
-export([is_core_function/1]).

-include("kapok.hrl").


is_core_function("+") -> {erlang, '+'};
is_core_function("*") -> {erlang, '*'};
is_core_function("self") -> {erlang, self};
is_core_function(_) -> false.

