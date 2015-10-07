-module(scanner_test).
-include_lib("eunit/include/eunit.hrl").

scan(String) ->
    {ok, Tokens, _Location} = ceiba_scanner:scan(String, 1, []),
    Tokens.

hex_bin_octal_test() ->
    ?assertEqual([{number, {1, 1}, 255}], scan("0xFF")).

