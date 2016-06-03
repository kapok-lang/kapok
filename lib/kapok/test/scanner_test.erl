-module(scanner_test).
-include_lib("eunit/include/eunit.hrl").

scan(String) ->
  {ok, Tokens, _Location} = kapok_scanner:scan(String, 1, []),
  Tokens.

scan_error(String) ->
  {error, ErrorInfo, _, _} = kapok_scanner:scan(String, 1, []),
  {_, Module, ErrorDesc} = ErrorInfo,
  Module:format_error(ErrorDesc).

unquoted_atom_test() ->
  ?assertEqual([{atom, [{line,1},{column,1}], '!'}], scan(":!")),
  ?assertEqual([{atom, [{line,1},{column,1}], '#'}], scan(":#")),
  ?assertEqual([{atom, [{line,1},{column,1}], '$'}], scan(":$")),
  ?assertEqual([{atom, [{line,1},{column,1}], '*'}], scan(":*")),
  ?assertEqual([{atom, [{line,1},{column,1}], 't+'}], scan(":t+")),
  ?assertEqual([{atom, [{line,1},{column,1}], 't-'}], scan(":t-")),
  ?assertEqual([{atom, [{line,1},{column,1}], '<'}], scan(":<")),
  ?assertEqual([{atom, [{line,1},{column,1}], '='}], scan(":=")),
  ?assertEqual([{atom, [{line,1},{column,1}], '>'}], scan(":>")),
  ?assertEqual([{atom, [{line,1},{column,1}], '>'}], scan(":>")),
  ?assertEqual([{atom, [{line,1},{column,1}], '?'}], scan(":?")),
  ?assertEqual([{atom, [{line,1},{column,1}], '@'}], scan(":@")),
  ?assertEqual([{atom, [{line,1},{column,1}], '^'}], scan(":^")),
  ?assertEqual([{atom, [{line,1},{column,1}], '_'}], scan(":_")),
  ?assertEqual([{atom, [{line,1},{column,1}], '|'}], scan(":|")).

quoted_atom_test() ->
  ?assertEqual([{atom_unsafe, [{line,1},{column,1}], <<"foo bar">>}], scan(":\"foo bar\"")).

atom_test() ->
  ?assertEqual([{atom, [{line,1},{column,1}], f0_1}], scan(":f0_1")).

char_test() ->
  ?assertEqual([{char_number, [{line,1},{column,1}], 97}], scan("\\a")),
  ?assertEqual([{char_number, [{line,1},{column,1}], 99}], scan("\\c")),
  ?assertEqual([{char_number, [{line,1},{column,1}], 10}], scan("\\newline")),
  ?assertEqual([{char_number, [{line,1},{column,1}], 92}], scan("\\\\")),
  ?assertEqual([{char_number, [{line,1},{column,1}], 0}], scan("\\x0")),
  ?assertEqual([{char_number, [{line,1},{column,1}], 7}], scan("\\x7")),
  ?assertEqual([{char_number, [{line,1},{column,1}], 10}], scan("\\xa")),
  ?assertEqual([{char_number, [{line,1},{column,1}], 12}], scan("\\xc")),
  ?assertEqual([{char_number, [{line,1},{column,1}], 10}], scan("\\x{a}")),
  ?assertEqual([{char_number, [{line,1},{column,1}], 171}], scan("\\x{ab}")),
  ?assertEqual([{char_number, [{line,1},{column,1}], 2748}], scan("\\x{abc}")),
  ?assertEqual([{char_number, [{line,1},{column,1}], 43981}], scan("\\x{abcd}")),
  ?assertEqual([{char_number, [{line,1},{column,1}], 703710}], scan("\\x{abcde}")),
  ?assertEqual([{char_number, [{line,1},{column,1}], 1092557}], scan("\\x{10abcd}")).

integer_test() ->
  ?assertEqual([{integer, [{line,1},{column,1}], 123}], scan("123")),
  ?assertEqual([{integer, [{line,1},{column,1}], 123}, {',', [{line,1},{column,4}]}], scan("123,")),
  ?assertEqual([{integer, [{line,2},{column,1}], 123}], scan("\n123\n")),
  ?assertEqual([{integer, [{line,1},{column,3}], 123}, {integer, [{line,1},{column,8}], 234}], scan("  123  234  ")).

hex_octal_n_base_test() ->
  ?assertEqual([{hex_number, [{line,1},{column,1}], 255}], scan("0xFF")),
  ?assertEqual([{octal_number, [{line,1},{column,1}], 63}], scan("077")),
  ?assertEqual([{n_base_number, [{line,1},{column,1}], 3}], scan("2r11")).

float_test() ->
  ?assertEqual([{float, [{line,1},{column,1}], 12.3}], scan("12.3")),
  ?assertEqual([{float, [{line,1},{column,1}], 12.3}, {',', [{line,1},{column,5}]}], scan("12.3,")),
  ?assertEqual([{float, [{line,2},{column,1}], 12.3}], scan("\n12.3\n")),
  ?assertEqual([{float, [{line,1},{column,3}], 12.3}, {float, [{line,1},{column,9}], 23.4}], scan("  12.3  23.4  ")).

scientific_test() ->
  ?assertEqual([{float, [{line,1},{column,1}], 0.1}], scan("1.0e-1")).

signed_number_test() ->
  ?assertEqual([{'+', [{line,1},{column,1}]}, {integer, [{line,1},{column,3}], 234}], scan("+ 234")),
  ?assertEqual([{'-', [{line,1},{column,1}]}, {float, [{line,1},{column,3}], 23.4}], scan("- 23.4")).

comment_test() ->
  ?assertEqual([{integer, [{line,1},{column,1}], 1}, {integer, [{line,2},{column,1}], 2}], scan("1 ;; comment\n2")),
  ?assertEqual([{integer, [{line,1},{column,1}], 1}, {integer, [{line,3},{column,1}], 2}], scan("1\n;; comment ...\n2")).

identifier_test() ->
  ?assertEqual([{identifier, [{line,1},{column,1}], 'abc'}], scan("abc ")),
  ?assertEqual([{identifier, [{line,1},{column,1}], 'Tp!#$*+=<=>?@^_|'}], scan("Tp!#$*+=<=>?@^_|")).

dot_test() ->
  ?assertEqual([{identifier, [{line,1},{column,1}], 'foo'},
                {'.', [{line,1},{column,4}]},
                {identifier, [{line,1},{column,5}], 'bar'},
                {'.', [{line,1},{column,8}]},
                {identifier, [{line,1},{column,9}], 'baz'}],
               scan("foo.bar.baz")).

newline_test() ->
  ?assertEqual([{identifier, [{line,1},{column,1}], 'foo'},
                {'.', [{line,2},{column,1}]},
                {identifier, [{line,2},{column,2}], 'bar'}],
               scan("foo\n.bar")),
  ?assertEqual([{integer, [{line,1},{column,1}], 1},
                {unquote_splicing, [{line,2},{column,1}]},
                {integer, [{line,2},{column,3}], 2}],
               scan("1\n~@2")).

string_test() ->
  ?assertEqual([{binary_string, [{line,1},{column,1}], <<"foo">>}], scan("\"foo\"")),
  ?assertEqual([{binary_string, [{line,1},{column,1}], <<"f\"">>}], scan("\"f\\\"\"")),
  ?assertEqual([{list_string, [{line,1},{column,1}], <<"foo">>}], scan("'foo'")),
  %% empty string
  ?assertEqual([{binary_string, [{line,1},{column,1}], <<>>}], scan("\"\"")),
  ?assertEqual([{list_string, [{line,1},{column,1}], <<>>}], scan("''")).

space_test() ->
  %% valid
  ?assertEqual([{identifier, [{line,1},{column,1}], 'foo'}, {integer, [{line,1},{column,5}], 2}], scan("foo 2")),
  ?assertEqual([{identifier, [{line,1},{column,1}], 'foo'}, {integer, [{line,1},{column,6}], 2}], scan("foo  2")),
  %% invalid
  Expect = kapok_scanner:format_error({invalid_space, 16#A0, "2"}),
  ?assertEqual(Expect, scan_error("foo" ++ [16#A0] ++"2")).

container_test() ->
  %% bitstring
  ?assertEqual([{'<<', [{line,1},{column,2}]},
                {integer, [{line,1},{column,5}], 1},
                {'>>', [{line,1},{column,7}]}], scan(" << 1 >> ")),
  %% list
  ?assertEqual([{'(', [{line,1},{column,2}]},
                {integer, [{line,1},{column,4}], 1},
                {')', [{line,1},{column,6}]}], scan(" ( 1 ) ")),
  ?assertEqual([{'[', [{line,1},{column,2}]},
                {integer, [{line,1},{column,4}], 1},
                {']', [{line,1},{column,6}]}], scan(" [ 1 ] ")),
  %% tuple
  ?assertEqual([{'{', [{line,1},{column,2}]},
                {integer, [{line,1},{column,4}], 1},
                {'}', [{line,1},{column,6}]}], scan(" { 1 } ")),
  %% map
  ?assertEqual([{'#{', [{line,1},{column,2}]},
                {integer, [{line,1},{column,5}], 1},
                {'}', [{line,1},{column,7}]}], scan(" #{ 1 } ")),
  %% set
  ?assertEqual([{'%{', [{line,1},{column,2}]},
                {integer, [{line,1},{column,5}], 1},
                {'}', [{line,1},{column,7}]}], scan(" %{ 1 } ")).

