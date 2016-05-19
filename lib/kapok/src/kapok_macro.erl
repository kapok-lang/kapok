%% special forms and macros
-module(kapok_macro).

-export([quote/1]).

-include("kapok.hrl").

%% Quotes an expression and return its AST.

%% tuple
quote(Arg) when is_tuple(Arg) ->
  {tuple, [], lists:map(fun quote/1, tuple_to_list(Arg))};

%% list

quote(Args) when is_list(Args) ->
  {list, [], lists:map(fun quote/1, Args)};

%% map
quote(Arg) when is_map(Arg) ->
  {map, [], maps:to_list(Arg)};

%% atom

quote(Arg) when is_atom(Arg) ->
  {atom, [], Arg};

%% number
quote(Number) when is_number(Number) ->
  {number, [], Number};

%% list string and binary string
quote(Arg) when is_binary(Arg) ->
  {bitstring, [], {binary_string, [], Arg}}.

