-module(scanner_test).
-include_lib("eunit/include/eunit.hrl").

scan(String) ->
    {ok, Tokens, _Location} = ceiba_scanner:scan(String, 1, []),
    Tokens.

scan_error(String) ->
    {error, ErrorInfo, _, _} = ceiba_scanner:scan(String, 1, []),
    {_, Module, ErrorDesc} = ErrorInfo,
    Module:format_error(ErrorDesc).

unquoted_atom_test() ->
    ?assertEqual([{atom, {1,1}, '!'}], scan(":!")),
    ?assertEqual([{atom, {1,1}, '#'}], scan(":#")),
    ?assertEqual([{atom, {1,1}, '$'}], scan(":$")),
    ?assertEqual([{atom, {1,1}, '*'}], scan(":*")),
    ?assertEqual([{atom, {1,1}, '+'}], scan(":+")),
    ?assertEqual([{atom, {1,1}, '-'}], scan(":-")),
    ?assertEqual([{atom, {1,1}, '/'}], scan(":/")),
    ?assertEqual([{atom, {1,1}, '<'}], scan(":<")),
    ?assertEqual([{atom, {1,1}, '='}], scan(":=")),
    ?assertEqual([{atom, {1,1}, '>'}], scan(":>")),
    ?assertEqual([{atom, {1,1}, '>'}], scan(":>")),
    ?assertEqual([{atom, {1,1}, '?'}], scan(":?")),
    ?assertEqual([{atom, {1,1}, '@'}], scan(":@")),
    ?assertEqual([{atom, {1,1}, '^'}], scan(":^")),
    ?assertEqual([{atom, {1,1}, '_'}], scan(":_")),
    ?assertEqual([{atom, {1,1}, '|'}], scan(":|")).

quoted_atom_test() ->
    ?assertEqual([{atom_unsafe, {1,1}, [<<"foo bar">>]}], scan(":\"foo bar\"")).

atom_test() ->
    ?assertEqual([{atom, {1,1}, f0_1}], scan(":f0_1")).

char_test() ->
    ?assertEqual([{number, {1,1}, 97}], scan("\\a")),
    ?assertEqual([{number, {1,1}, 99}], scan("\\c")),
    ?assertEqual([{number, {1,1}, 10}], scan("\\newline")),
    ?assertEqual([{number, {1,1}, 92}], scan("\\\\")),
    ?assertEqual([{number, {1,1}, 0}], scan("\\x0")),
    ?assertEqual([{number, {1,1}, 7}], scan("\\x7")),
    ?assertEqual([{number, {1,1}, 10}], scan("\\xa")),
    ?assertEqual([{number, {1,1}, 12}], scan("\\xc")),
    ?assertEqual([{number, {1,1}, 10}], scan("\\x{a}")),
    ?assertEqual([{number, {1,1}, 171}], scan("\\x{ab}")),
    ?assertEqual([{number, {1,1}, 2748}], scan("\\x{abc}")),
    ?assertEqual([{number, {1,1}, 43981}], scan("\\x{abcd}")),
    ?assertEqual([{number, {1,1}, 703710}], scan("\\x{abcde}")),
    ?assertEqual([{number, {1,1}, 1092557}], scan("\\x{10abcd}")).

integer_test() ->
    ?assertEqual([{number, {1,1}, 123}], scan("123")),
    ?assertEqual([{number, {1,1}, 123}, {',', {1,4}}], scan("123,")),
    ?assertEqual([{number, {2,1}, 123}], scan("\n123\n")),
    ?assertEqual([{number, {1,3}, 123}, {number, {1,8}, 234}], scan("  123  234  ")).

hex_bin_octal_test() ->
    ?assertEqual([{number, {1,1}, 255}], scan("0xFF")),
    ?assertEqual([{number, {1,1}, 63}], scan("077")),
    ?assertEqual([{number, {1,1}, 3}], scan("2r11")).

float_test() ->
    ?assertEqual([{number, {1,1}, 12.3}], scan("12.3")),
    ?assertEqual([{number, {1,1}, 12.3}, {',', {1,5}}], scan("12.3,")),
    ?assertEqual([{number, {2,1}, 12.3}], scan("\n12.3\n")),
    ?assertEqual([{number, {1,3}, 12.3}, {number, {1,9}, 23.4}], scan("  12.3  23.4  ")).

scientific_test() ->
    ?assertEqual([{number, {1,1}, 0.1}], scan("1.0e-1")).

comment_test() ->
    ?assertEqual([{number, {1,1}, 1}, {number, {2,1}, 2}], scan("1 ;; comment\n2")),
    ?assertEqual([{number, {1,1}, 1}, {number, {3,1}, 2}], scan("1\n;; comment ...\n2")).

identifier_test() ->
    ?assertEqual([{identifier, {1,1}, "abc"}], scan("abc ")),
    ?assertEqual([{identifier, {1,1}, "Tp!#$*+=/<=>?@^_|"}], scan("Tp!#$*+=/<=>?@^_|")).

dot_test() ->
    ?assertEqual([{identifier, {1,1}, "foo"},
                  {'.', {1,4}},
                  {identifier, {1,5}, "bar"},
                  {'.', {1,8}},
                  {identifier, {1,9}, "baz"}],
                 scan("foo.bar.baz")).

newline_test() ->
    ?assertEqual([{identifier, {1,1}, "foo"},
                  {'.', {2,1}},
                  {identifier, {2,2}, "bar"}],
                 scan("foo\n.bar")),
    ?assertEqual([{number, {1,1}, 1},
                  {unquote_splicing, {2,1}},
                  {number, {2,3}, 2}],
                 scan("1\n~@2")).

string_test() ->
    ?assertEqual([{binary_string, {1,1}, [<<"foo">>]}], scan("\"foo\"")),
    ?assertEqual([{binary_string, {1,1}, [<<"f\"">>]}], scan("\"f\\\"\"")),
    ?assertEqual([{list_string, {1,1}, [<<"foo">>]}], scan("'foo'")),
    %% empty string
    ?assertEqual([{binary_string, {1,1}, [<<>>]}], scan("\"\"")),
    ?assertEqual([{list_string, {1,1}, [<<>>]}], scan("''")).

space_test() ->
    %% valid
    ?assertEqual([{identifier, {1,1}, "foo"}, {number, {1,5}, 2}], scan("foo 2")),
    ?assertEqual([{identifier, {1,1}, "foo"}, {number, {1,6}, 2}], scan("foo  2")),
    %% invalid
    Expect = ceiba_scanner:format_error({invalid_space, 16#A0, "2"}),
    ?assertEqual(Expect, scan_error("foo" ++ [16#A0] ++"2")).

container_test() ->
    %% binary
    ?assertEqual([{'<<', {1, 2}},
                  {number, {1, 5}, 1},
                  {'>>', {1, 7}}], scan(" << 1 >> ")),
    %% list
    ?assertEqual([{'(', {1, 2}},
                  {number, {1, 4}, 1},
                  {')', {1, 6}}], scan(" ( 1 ) ")),
    ?assertEqual([{'[', {1, 2}},
                  {number, {1, 4}, 1},
                  {']', {1, 6}}], scan(" [ 1 ] ")),
    %% tuple
    ?assertEqual([{'{', {1, 2}},
                  {number, {1, 4}, 1},
                  {'}', {1, 6}}], scan(" { 1 } ")),
    %% map
    ?assertEqual([{'#{', {1, 2}},
                  {number, {1, 5}, 1},
                  {'}', {1, 7}}], scan(" #{ 1 } ")),
    %% set
    ?assertEqual([{'%{', {1, 2}},
                  {number, {1, 5}, 1},
                  {'}', {1, 7}}], scan(" %{ 1 } ")).

