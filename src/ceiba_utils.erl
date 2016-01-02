%% Convenience functions used throughout ceiba source code.

-module(ceiba_utils).
-export([get_line/1,
         characters_to_list/1]).

get_line(Opts) when is_list(Opts) ->
    case lists:keyfind(line, 1, Opts) of
        {line, Line} when is_integer(Line) -> Line;
        false -> 0
    end.

characters_to_list(Data) when is_list(Data) ->
    Data;
characters_to_list(Data) ->
    unicode:characters_to_list(Data).

