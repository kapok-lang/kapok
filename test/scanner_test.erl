-module(scanner_test).
-include_lib("eunit/include/eunit.hrl").

scan(String) ->
    {ok, _Line, _Column, Result} = ceiba_scanner:scan(String, 1, []),
    Result.

hex_bin_octal_test() ->
    [{number, {{1, 1}, {1, 5}}, 255}] = scan("0xFF").
