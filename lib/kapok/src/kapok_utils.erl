%% Helper functions used throughout kapok source code.

-module(kapok_utils).
-export([get_line/1,
         characters_to_list/1,
         chardata_to_string/1,
         to_binary/1]).

get_line(Opts) when is_list(Opts) ->
  case lists:keyfind(line, 1, Opts) of
    {line, Line} when is_integer(Line) -> Line;
    false -> 0
  end.

characters_to_list(List) when is_list(List) ->
  List;
characters_to_list(Data) ->
  unicode:characters_to_list(Data).

chardata_to_string(Bin) when is_binary(Bin) ->
  Bin;
chardata_to_string(List) when is_list(List) ->
  case unicode:characters_to_binary(List) of
    Result when is_binary(Result) ->
      Result;
    {error, Encoded, Rest} ->
      throw({invalid, Encoded, Rest});
    {incomplete, Encoded, Rest} ->
      throw({incomplete, Encoded, Rest})
  end.

to_binary(List) when is_list(List) -> unicode:characters_to_binary(List);
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8).


