%%
-module(kapok_scanner).
-export([token_category/1,
         token_meta/1,
         token_symbol/1,
         scan/3,
         scan/4,
         format_error/1]).
-include("kapok.hrl").

-type category() :: atom().
-type line() :: integer().
-type column() :: pos_integer().
-type meta() :: [{line, line()} | {column, column()}].
-type location() :: {line(), column()}.
-type symbol() :: atom() | float() | integer() | string() | binary().
-type token() :: {category(), meta(), symbol()}
               | {category(), meta()}.
-type tokens() :: [token()].
-type option() :: {atom(), term()}.
-type options() :: [option()].
-type error_info() :: {location(), module(), term()}.

-spec token_category(Token :: token()) -> Return :: category().
token_category(Token) ->
  element(1, Token).
-spec token_meta(Token :: token()) -> Return :: meta().
token_meta(Token) ->
  element(2, Token).
-spec token_symbol(Token :: token()) -> Return :: symbol().
token_symbol(Token) ->
  case Token of
    {_, _, Symbol} ->
      Symbol;
    {_, _} ->
      nil
  end.

build_meta(Line, Column) ->
  [{line, Line}, {column, Column}].

%% start of scan()

-type tokens_result() :: {'ok', Tokens :: tokens(), EndLocation :: location()}
                       | {'error', ErrorInfo :: error_info(), Rest :: string(),
                          Tokens :: tokens()}.
-spec scan(String, Line, Options) -> Return when String :: string(),
                                                 Line :: integer(),
                                                 Options :: options(),
                                                 Return :: tokens_result().
scan(String, Line, Options) ->
  scan(String, Line, 1, Options).

-spec scan(String, Line, Column, Options) -> Return when String :: string(),
                                                         Line :: integer(),
                                                         Column :: pos_integer(),
                                                         Options :: options(),
                                                         Return :: tokens_result().
scan(String, Line, Column, Options) ->
  File = case lists:keyfind(file, 1, Options) of
           {file, F} -> F;
           false -> <<"nofile">>
         end,
  Check = case lists:keyfind(check_terminators, 1, Options) of
            {check_terminators, false} -> false;
            false -> true
          end,
  Existing = case lists:keyfind(existing_atoms_only, 1, Options) of
               {existing_atoms_only, true} -> true;
               false -> false
             end,
  Scope = #kapok_scanner_scope{
               file = File,
               check_terminators = Check,
               existing_atoms_only = Existing},
  scan(String, Line, Column, Scope, []).

%% success
scan([], Line, Column, #kapok_scanner_scope{terminators=[]}, Tokens) ->
  {ok, lists:reverse(Tokens), build_meta(Line, Column)};

%% terminator missing
scan([], EndLine, Column,
     #kapok_scanner_scope{terminators=[{Open, {{OpenLine, _} ,_}}|_]},
     Tokens) ->
  Close = terminator(Open),
  {error, {{EndLine, Column}, ?MODULE, {missing_terminator, Close, Open, OpenLine}}, [],
   lists:reverse(Tokens)};

%% Base integers

%% hex
scan([S, $0, $x, H|T], Line, Column, Scope, Tokens) when ?is_sign(S), ?is_hex(H) ->
  do_scan_hex({list_to_atom([S]), hex_number}, 3, [H|T], Line, Column, Scope, Tokens);
scan([$0, $x, H|T], Line, Column, Scope, Tokens) when ?is_hex(H) ->
  do_scan_hex(hex_number, 2, [H|T], Line, Column, Scope, Tokens);

%% octal
scan([S, $0, H|T], Line, Column, Scope, Tokens) when ?is_sign(S), ?is_octal(H) ->
  do_scan_octal({list_to_atom([S]), octal_number}, 2, [H|T], Line, Column, Scope, Tokens);
scan([$0, H|T], Line, Column, Scope, Tokens) when ?is_octal(H) ->
  do_scan_octal(octal_number, 1, [H|T], Line, Column, Scope, Tokens);

%% flexible N(2 - 36) numeral bases
scan([S, B1, $r, H|T], Line, Column, Scope, Tokens) when ?is_sign(S), (B1 >= $2 andalso B1 =< $9) ->
  N = B1 - $0,
  do_scan_n_base({list_to_atom([S]), n_base_number}, 3, N, H, T, Line, Column, Scope, Tokens);
scan([B1, $r, H|T], Line, Column, Scope, Tokens) when (B1 >= $2 andalso B1 =< $9) ->
  N = B1 - $0,
  do_scan_n_base(n_base_number, 2, N, H, T, Line, Column, Scope, Tokens);
scan([S, B1, B2, $r, H|T], Line, Column, Scope, Tokens)
    when ?is_sign(S), (B1 >= $1 andalso B1 =< $2), (B2 >= $0 andalso B2 =< $9);
         (B1 == $3), (B2 >= $0 andalso B2 =< $6) ->
  N = list_to_integer([B1, B2]),
  do_scan_n_base({list_to_atom([S]), n_base_number}, 3, N, H, T, Line, Column, Scope, Tokens);
scan([B1, B2, $r, H|T], Line, Column, Scope, Tokens)
    when (B1 >= $1 andalso B1 =< $2), (B2 >= $0 andalso B2 =< $9);
         (B1 == $3), (B2 >= $0 andalso B2 =< $6) ->
  N = list_to_integer([B1, B2]),
  do_scan_n_base(n_base_number, 3, N, H, T, Line, Column, Scope, Tokens);

%% Comment

scan([$;|T], Line, Column, Scope, Tokens) ->
  Rest = scan_comment(T),
  scan(Rest, Line, Column, Scope, Tokens);

%% Char

scan([$\\, $x, ${,A,B,C,D,E,F,$}|T], Line, Column, Scope, Tokens)
    when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E), ?is_hex(F) ->
  Char = escape_char([$\\, $x, ${,A,B,C,D,E,F,$}]),
  scan(T, Line, Column+10, Scope, [{char_number, build_meta(Line, Column), Char}|Tokens]);

scan([$\\, $x, ${,A,B,C,D,E,$}|T], Line, Column, Scope, Tokens)
    when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E) ->
  Char = escape_char([$\\, $x, ${,A,B,C,D,E,$}]),
  scan(T, Line, Column+9, Scope, [{char_number, build_meta(Line, Column), Char}|Tokens]);

scan([$\\, $x, ${,A,B,C,D,$}|T], Line, Column, Scope, Tokens)
    when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D) ->
  Char = escape_char([$\\, $x, ${,A,B,C,D,$}]),
  scan(T, Line, Column + 8, Scope, [{char_number, build_meta(Line, Column), Char}|Tokens]);

scan([$\\, $x, ${,A,B,C,$}|T], Line, Column, Scope, Tokens)
    when ?is_hex(A), ?is_hex(B), ?is_hex(C) ->
  Char = escape_char([$\\, $x, ${,A,B,C,$}]),
  scan(T, Line, Column + 7, Scope, [{char_number, build_meta(Line, Column), Char}|Tokens]);

scan([$\\, $x, ${,A,B,$}|T], Line, Column, Scope, Tokens) when ?is_hex(A), ?is_hex(B) ->
  Char = escape_char([$\\, $x, ${,A,B,$}]),
  scan(T, Line, Column + 6, Scope, [{char_number, build_meta(Line, Column), Char}|Tokens]);

scan([$\\, $x, ${,A,$}|T], Line, Column, Scope, Tokens) when ?is_hex(A) ->
  Char = escape_char([$\\, $x, ${,A,$}]),
  scan(T, Line, Column + 5, Scope, [{char_number, build_meta(Line, Column), Char}|Tokens]);

scan([$\\, $x, A, B|T], Line, Column, Scope, Tokens) when ?is_hex(A), ?is_hex(B) ->
  Char = escape_char([$\\, $x, A, B]),
  scan(T, Line, Column + 4, Scope, [{char_number, build_meta(Line, Column), Char}|Tokens]);

scan([$\\, $x, A|T], Line, Column, Scope, Tokens) when ?is_hex(A) ->
  Char = escape_char([$\\, $x, A]),
  scan(T, Line, Column + 3, Scope, [{char_number, build_meta(Line, Column), Char}|Tokens]);

scan([$\\, $s,$p,$a,$c,$e|T], Line, Column, Scope, Tokens) ->
  Char = $\s,
  scan(T, Line, Column + 6, Scope, [{char_number, build_meta(Line, Column), Char}|Tokens]);

scan([$\\, $t,$a,$b|T], Line, Column, Scope, Tokens) ->
  Char = $\t,
  scan(T, Line, Column + 4, Scope, [{char_number, build_meta(Line, Column), Char}|Tokens]);

scan([$\\, $f,$o,$r,$m,$f,$e,$e,$d|T], Line, Column, Scope, Tokens) ->
  Char = $\f,
  scan(T, Line, Column + 9, Scope, [{char_number, build_meta(Line, Column), Char}|Tokens]);

scan([$\\, $b,$a,$c,$k,$s,$p,$a,$c,$e|T], Line, Column, Scope, Tokens) ->
  Char = $\b,
  scan(T, Line, Column+10, Scope, [{char_number, build_meta(Line, Column), Char}|Tokens]);

scan([$\\, $n,$e,$w,$l,$i,$n,$e|T], Line, Column, Scope, Tokens) ->
  Char = $\n,
  scan(T, Line, Column + 8, Scope, [{char_number, build_meta(Line, Column), Char}|Tokens]);

scan([$\\, $r,$e,$t,$u,$r,$n|T], Line, Column, Scope, Tokens) ->
  Char = $\r,
  scan(T, Line, Column + 7, Scope, [{char_number, build_meta(Line, Column), Char}|Tokens]);

%% End of line

scan("\\" = Original, Line, Column, _Scope, Tokens) ->
  {error, {{Line, Column}, ?MODULE, {invalid_eof, escape}}, Original,
   lists:reverse(Tokens)};
scan("\\\n" = Original, Line, Column, _Scope, Tokens) ->
  {error, {{Line, Column}, ?MODULE, {invalid_eof, escape}}, Original,
   lists:reverse(Tokens)};
scan("\\\r\n" = Original, Line, Column, _Scope, Tokens) ->
  {error, {{Line, Column}, ?MODULE, {invalid_eof, escape}}, Original,
   lists:reverse(Tokens)};
scan("\\\n" ++ T, Line, _Column, Scope, Tokens) ->
  scan(T, Line + 1, 1, Scope, Tokens);
scan("\\\r\n" ++ T, Line, _Column, Scope, Tokens) ->
  scan(T, Line + 1, 1, Scope, Tokens);
scan("\n" ++ T, Line, _Column, Scope, Tokens) ->
  scan(T, Line + 1, 1, Scope, Tokens);
scan("\r\n" ++ T, Line, _Column, Scope, Tokens) ->
  scan(T, Line + 1, 1, Scope, Tokens);

%% Escaped chars

scan([$\\, H|T], Line, Column, Scope, Tokens) ->
  Char = unescape_map(H),
  scan(T, Line, Column + 2, Scope, [{char_number, build_meta(Line, Column), Char}|Tokens]);

%% Strings

%% heredoc
scan("\"\"\"" ++ T, Line, Column, Scope, Tokens) ->
  handle_string(T, Line, Column, [$", $", $"], Scope, Tokens);
scan("'''" ++ T, Line, Column, Scope, Tokens) ->
  handle_string(T, Line, Column, [$', $', $'], Scope, Tokens);

%% string
scan([$"|T], Line, Column, Scope, Tokens) ->
  handle_string(T, Line, Column + 1, [$"], Scope, Tokens);
scan([$'|T], Line, Column, Scope, Tokens) ->
  handle_string(T, Line, Column + 1, [$'], Scope, Tokens);

%% Atoms

scan([$:, H|T] = Original, Line, Column, Scope, Tokens) when ?is_quote(H) ->
  case scan_string(Line, Column + 2, T, H) of
    {ok, NewLine, NewColumn, Bin, Rest} ->
      case unescape_token(Bin) of
        {ok, Unescaped} ->
          Tag = case Scope#kapok_scanner_scope.existing_atoms_only of
                  true -> atom_safe;
                  false -> atom_unsafe
                end,
          scan(Rest, NewLine, NewColumn, Scope,
               [{Tag, build_meta(Line, Column), Unescaped}|Tokens]);
        {error, ErrorDescription} ->
          {error, {{Line, Column}, ?MODULE, ErrorDescription},
           Original, lists:reverse(Tokens)}
      end;
    {error, Location, _}->
      {error, {Location, ?MODULE, {missing_terminator, H, H, Line}},
       Original, lists:reverse(Tokens)}
  end;
scan([$:, H|T], Line, Column, Scope, Tokens) when ?is_identifier_start(H) ->
  handle_atom([H|T], Line, Column, Scope, Tokens);

%% Containers

%% Bitstring
scan([H, H|T], Line, Column, Scope, Tokens) when H == $<; H == $< ->
  Token = {list_to_atom([H, H]), build_meta(Line, Column)},
  handle_terminator(T, Line, Column + 2, Scope, Token, Tokens);

scan([H, H|T], Line, Column, Scope, Tokens) when H == $>; H == $> ->
  Token = {list_to_atom([H, H]), build_meta(Line, Column)},
  handle_terminator(T, Line, Column + 2, Scope, Token, Tokens);

%% map, set
scan([H1, H2|T], Line, Column, Scope, Tokens)
    when H1 == $#, H2 == ${; H1 == $%, H2 == ${ ->
  Token = {list_to_atom([H1, H2]), build_meta(Line, Column)},
  handle_terminator(T, Line, Column + 2, Scope, Token, Tokens);

%% List, tuple
scan([H|T], Line, Column, Scope, Tokens)
    when H == $(; H == $); H == $[; H == $]; H == ${; H == $} ->
  NextColumn = Column + 1,
  Token = {list_to_atom([H]), build_meta(Line, Column)},
  handle_terminator(T, Line, NextColumn, Scope, Token, Tokens);

%% two chars operators

scan([$~, $@|T], Line, Column, Scope, Tokens) ->
  scan(T, Line, Column + 2, Scope, [{unquote_splicing, build_meta(Line, Column)}|Tokens]);

%% one char operators

scan([$`|T], Line, Column, Scope, Tokens) ->
  scan(T, Line, Column + 1, Scope, [{backquote, build_meta(Line, Column)}|Tokens]);

scan([$'|T], Line, Column, Scope, Tokens) ->
  scan(T, Line, Column + 1, Scope, [{quote, build_meta(Line, Column)}|Tokens]);

scan([$~|T], Line, Column, Scope, Tokens) ->
  scan(T, Line, Column + 1, Scope, [{unquote, build_meta(Line, Column)}|Tokens]);

scan([$&|T], Line, Column, Scope, Tokens) ->
  scan(T, Line, Column + 1, Scope, [{rest, build_meta(Line, Column)}|Tokens]);

scan([$,|T], Line, Column, Scope, Tokens) ->
  scan(T, Line, Column + 1, Scope, [{',', build_meta(Line, Column)}|Tokens]);

%% Others

scan([$.|T], Line, Column, Scope, Tokens) ->
  scan(T, Line, Column + 1, Scope, [{'.', build_meta(Line, Column)}|Tokens]);

%% Integers and floats

scan([S, H|T], Line, Column, Scope, Tokens) when ?is_sign(S), ?is_digit(H) ->
  do_scan_number(list_to_atom([S]), 1, [H|T], Line, Column, Scope, Tokens);
scan([H|_] = Original, Line, Column, Scope, Tokens) when ?is_digit(H) ->
  do_scan_number(nil, 0, Original, Line, Column, Scope, Tokens);

%% Identifiers

scan([H|_] = Original, Line, Column, Scope, Tokens) when ?is_identifier_start(H) ->
  handle_identifier(Original, Line, Column, Scope, Tokens);

%% Spaces
scan([H|T], Line, Column, Scope, Tokens) when ?is_horizontal_space(H) ->
  scan(T, Line, Column + 1, Scope, Tokens);

scan([H|T] = Original, Line, Column, _Scope, Tokens) when ?is_invalid_space(H) ->
  {error, {{Line, Column}, ?MODULE, {invalid_space, H, until_eol(T)}},
   Original, lists:reverse(Tokens)};

scan(T, Line, Column, _Scope, Tokens) ->
  {error, {{Line, Column}, ?MODULE, {invalid_token, until_eol(T)}}, T,
   lists:reverse(Tokens)}.

%% end of scan()

until_eol(Rest) ->
  until_eol(Rest, []).
until_eol("\r\n" ++ _, Acc) -> lists:reverse(Acc);
until_eol("\n" ++ _, Acc)   -> lists:reverse(Acc);
until_eol([], Acc)          -> lists:reverse(Acc);
until_eol([H|T], Acc)       -> until_eol(T, [H|Acc]).

%% Integers and floats
%% helpers

do_scan_hex(Flag, PrefixLength, String, Line, Column, Scope, Tokens) ->
  {Rest, Number, Length} = scan_hex(String, []),
  Token = case Flag of
            {Sign, Category} when is_atom(Sign), is_atom(Category) ->
              {Sign, build_meta(Line, Column), {Category, build_meta(Line, Column+1), Number}};
            Category when is_atom(Category) ->
              {Category, build_meta(Line, Column), Number}
            end,
  scan(Rest, Line, Column + PrefixLength + Length, Scope, [Token|Tokens]).

do_scan_octal(Flag, PrefixLength, String, Line, Column, Scope, Tokens) ->
  {Rest, Number, Length} = scan_octal(String, []),
  Token = case Flag of
            {Sign, Category} when is_atom(Sign), is_atom(Category) ->
              {Sign, build_meta(Line, Column), {Category, build_meta(Line, Column+1), Number}};
            Category when is_atom(Category) ->
              {Category, build_meta(Line, Column), Number}
            end,
  scan(Rest, Line, Column + PrefixLength + Length, Scope, [Token|Tokens]).

do_scan_n_base(Flag, PrefixLength, N, H, T, Line, Column, Scope, Tokens) ->
  case ?is_n_base(H, N) of
    true ->
      {Rest, Number, Length} = scan_n_base([H|T], N, []),
      Token = case Flag of
                {Sign, Category} when is_atom(Sign), is_atom(Category) ->
                  {Sign, build_meta(Line, Column), {Category, build_meta(Line, Column+1), Number}};
                Category when is_atom(Category) ->
                  {Category, build_meta(Line, Column), Number}
              end,
      scan(Rest, Line, Column + PrefixLength + Length, Scope, [Token|Tokens]);
    _ ->
      {error, {{Line, Column}, ?MODULE, {invalid_n_base_char, H, N, Line}},
       [], lists:reverse(Tokens)}
  end.

do_scan_number(Flag, PrefixLength, String, Line, Column, Scope, Tokens) ->
  {Rest, Category, Number, Length} = scan_number(String, [], false),
  Token = case Flag of
            nil ->
              {Category, build_meta(Line, Column), Number};
            Sign when is_atom(Sign) ->
              {Sign, build_meta(Line, Column), {Category, build_meta(Line, Column+1), Number}}
          end,
  scan(Rest, Line, Column + PrefixLength + Length, Scope, [Token|Tokens]).


%% At this point, we are at least sure the first digit is a number.

%% Check if we have a point followed by a number;
scan_number([$., H|T], Acc, false) when ?is_digit(H) ->
  scan_number(T, [H, $.|Acc], true);

%% Check if we have an underscore followed by a number;
scan_number([$_, H|T], Acc, Bool) when ?is_digit(H) ->
  scan_number(T, [H|Acc], Bool);

%% Check if we have e- followed by numbers (valid only for floats);
scan_number([E, S, H|T], Acc, true)
    when (E == $E) orelse (E == $e), ?is_digit(H), S == $+ orelse S == $- ->
  scan_number(T, [H, S, $e|Acc], true);

%% Check if we have e followed by numbers (valid only for floats);
scan_number([E, H|T], Acc, true)
    when (E == $E) orelse (E == $e), ?is_digit(H) ->
  scan_number(T, [H, $e|Acc], true);

%% Finally just numbers.
scan_number([H|T], Acc, Bool) when ?is_digit(H) ->
  scan_number(T, [H|Acc], Bool);

%% Cast to float...
scan_number(Rest, Acc, true) ->
  {Rest, float, list_to_float(lists:reverse(Acc)), length(Acc)};

%% Or integer.
scan_number(Rest, Acc, false) ->
  {Rest, integer, list_to_integer(lists:reverse(Acc)), length(Acc)}.

scan_hex([H|T], Acc) when ?is_hex(H) ->
  scan_hex(T, [H|Acc]);
scan_hex(Rest, Acc) ->
  {Rest, list_to_integer(lists:reverse(Acc), 16), length(Acc)}.

scan_octal([H|T], Acc) when ?is_octal(H) ->
  scan_octal(T, [H|Acc]);
scan_octal(Rest, Acc) ->
  {Rest, list_to_integer(lists:reverse(Acc), 8), length(Acc)}.

scan_n_base([H|T], N, Acc) when ?is_n_base(H, N) ->
  scan_n_base(T, N, [H|Acc]);
scan_n_base(Rest, N, Acc) ->
  {Rest, list_to_integer(lists:reverse(Acc), N), length(Acc)}.

%% Comment

scan_comment("\r\n" ++ _ = Rest) -> Rest;
scan_comment("\n" ++ _ = Rest) -> Rest;
scan_comment([_|Rest]) ->  scan_comment(Rest);
scan_comment([]) -> [].

%% Chars

escape_char(List) ->
  {ok, <<Char/utf8>>} = unescape_chars(list_to_binary(List)),
  Char.

unescape_token(Token) ->
  unescape_token(Token, fun unescape_map/1).
unescape_token(Token, Map) when is_binary(Token) -> unescape_chars(Token, Map);
unescape_token(Other, _Map) -> {ok, Other}.

%% Unescape chars.
%% For instance, "\" "n" (two chars) needs to be converted to "\n" (one char).

unescape_chars(String) ->
  unescape_chars(String, fun unescape_map/1).
unescape_chars(String, Map) ->
  unescape_chars(String, Map, Map($x) == true, <<>>).
unescape_chars(<<$\\, $x, A, B, Rest/binary>>, Map, true, Acc) when ?is_hex(A), ?is_hex(B) ->
  append_escaped(Rest, Map, [A, B], true, Acc, 16);
unescape_chars(<<$\\, $x, A, Rest/binary>>, Map, true, Acc) when ?is_hex(A) ->
  append_escaped(Rest, Map, [A], true, Acc, 16);
unescape_chars(<<$\\, $x, ${,A,$}, Rest/binary>>, Map, true, Acc) when ?is_hex(A) ->
  append_escaped(Rest, Map, [A], true, Acc, 16);
unescape_chars(<<$\\, $x, ${,A,B,$}, Rest/binary>>, Map, true, Acc) when ?is_hex(A), ?is_hex(B) ->
  append_escaped(Rest, Map, [A, B], true, Acc, 16);
unescape_chars(<<$\\, $x, ${,A,B,C,$}, Rest/binary>>, Map, true, Acc)
    when ?is_hex(A), ?is_hex(B), ?is_hex(C) ->
  append_escaped(Rest, Map, [A, B, C], true, Acc, 16);
unescape_chars(<<$\\, $x, ${,A,B,C,D,$}, Rest/binary>>, Map, true, Acc)
    when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D) ->
  append_escaped(Rest, Map, [A, B, C, D], true, Acc, 16);
unescape_chars(<<$\\, $x, ${,A,B,C,D,E,$}, Rest/binary>>, Map, true, Acc)
    when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E) ->
  append_escaped(Rest, Map, [A, B, C, D, E], true, Acc, 16);
unescape_chars(<<$\\, $x, ${,A,B,C,D,E,F,$}, Rest/binary>>, Map, true, Acc)
    when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E), ?is_hex(F) ->
  append_escaped(Rest, Map, [A, B, C, D, E, F], true, Acc, 16);
unescape_chars(<<$\\, $x, _/binary>>, _Map, true, _Acc) ->
  {error, {missing_hex_sequence}};
unescape_chars(<<$\\, Escaped, Rest/binary>>, Map, Hex, Acc) ->
  case Map(Escaped) of
    false -> unescape_chars(Rest, Map, Hex, <<Acc/binary, $\\, Escaped>>);
    Other -> unescape_chars(Rest, Map, Hex, <<Acc/binary, Other>>)
  end;
unescape_chars(<<Char, Rest/binary>>, Map, Hex, Acc) ->
  unescape_chars(Rest, Map, Hex, <<Acc/binary, Char>>);
unescape_chars(<<>>, _Map, _Hex, Acc) -> {ok, Acc}.

append_escaped(Rest, Map, List, Hex, Acc, Base) ->
  Codepoint = list_to_integer(List, Base),
  try <<Acc/binary, Codepoint/utf8>> of
      Binary -> unescape_chars(Rest, Map, Hex, Binary)
  catch
    error:badarg ->
      P = integer_to_binary(Codepoint),
      {error, {invalid_codepoint, P}}
  end.

%% Unescape Helpers

unescape_map($b) -> $\b;
unescape_map($d) -> $\d;
unescape_map($e) -> $\e;
unescape_map($f) -> $\f;
unescape_map($n) -> $\n;
unescape_map($r) -> $\r;
unescape_map($s) -> $\s;
unescape_map($t) -> $\t;
unescape_map($v) -> $\v;
unescape_map($x) -> true;
unescape_map(E)  -> E.

%% Strings
handle_string(T, Line, Column, Term, Scope, Tokens) ->
  case scan_string(Line, Column, T, Term) of
    {ok, NewLine, NewColumn, Bin, Rest} ->
      case unescape_token(Bin) of
        {ok, Unescaped} ->
          Token = {string_type(Term), build_meta(Line, Column-length(Term)), Unescaped},
          scan(Rest, NewLine, NewColumn, Scope, [Token|Tokens]);
        {error, ErrorDescription} ->
          {error, {{Line, Column}, ?MODULE, ErrorDescription}, Term ++ T, lists:reverse(Tokens)}
      end;
    {error, Location, _} ->
      {error, {Location, ?MODULE, {missing_terminator, Term, Term, Line}}, T, lists:reverse(Tokens)}
  end.

scan_string(Line, Column, T, Term) ->
  scan_string(Line, Column, T, Term, []).
scan_string(Line, Column, [], _Term, Acc) ->
  {error, {Line, Column}, lists:reverse(Acc)};
%% Terminators
%% multi-line string
scan_string(Line, Column, [C, C, C|Remaining], [C, C, C], Acc) ->
  String = unicode:characters_to_binary(lists:reverse(Acc)),
  {ok, Line, Column+3, String, Remaining};
%% string
scan_string(Line, Column, [C|Remaining], [C], Acc) ->
  String = unicode:characters_to_binary(lists:reverse(Acc)),
  {ok, Line, Column+1, String, Remaining};
%% Going through the string
scan_string(Line, _Column, [$\\, $\n|Rest], Term, Acc) ->
  scan_string(Line+1, 1, Rest, Term, Acc);
scan_string(Line, _Column, [$\\, $\r, $\n|Rest], Term, Acc) ->
  scan_string(Line+1, 1, Rest, Term, Acc);
scan_string(Line, _Column, [$\n|Rest], Term, Acc) ->
  scan_string(Line+1, 1, Rest, Term, [$\n|Acc]);
scan_string(Line, _Column, [$\r, $\n|Rest], Term, Acc) ->
  scan_string(Line+1, 1, Rest, Term, [$\n|Acc]);
scan_string(Line, Column, [$\\, C|Rest], [C] = Term, Acc) ->
  scan_string(Line, Column+2, Rest, Term, [C|Acc]);
scan_string(Line, Column, [$\\, Char|Rest], Term, Acc) ->
  scan_string(Line, Column+2, Rest, Term, [Char, $\\|Acc]);
%% Catch all clause
scan_string(Line, Column, [Char|Rest], Term, Acc) ->
  scan_string(Line, Column+1, Rest, Term, [Char|Acc]).

%% Identifiers

handle_atom(T, Line, Column, Scope, Tokens) ->
  case scan_identifier(Line, Column, T) of
    {ok, NewLine, NewColumn, Identifier, Rest} ->
      Atom = case Scope#kapok_scanner_scope.existing_atoms_only of
               true -> list_to_existing_atom(Identifier);
               false -> list_to_atom(Identifier)
             end,
      scan(Rest, NewLine, NewColumn, Scope, [{atom, build_meta(Line, Column), Atom}|Tokens]);
    {error, ErrorInfo} ->
      {error, ErrorInfo, [$: | T], Tokens}
  end.

handle_identifier(T, Line, Column, Scope, Tokens) ->
  case scan_identifier(Line, Column, T) of
    {ok, NewLine, NewColumn, Identifier, Rest} ->
      scan(Rest, NewLine, NewColumn, Scope,
           [{identifier, build_meta(Line, Column), list_to_atom(Identifier)}|Tokens]);
    {error, ErrorInfo} ->
      {error, ErrorInfo, T, lists:reverse(Tokens)}
  end.

scan_identifier(Line, Column, T) ->
  scan_identifier(Line, Column, T, []).
scan_identifier(Line, Column, [], Acc) ->
  {ok, Line, Column, lists:reverse(Acc), []};
%% bitstring terminators "<<" ">>" are always higher priority
scan_identifier(Line, Column, [A, B|_], [])
    when ((A == $<) andalso (B == $<)); ((A == $>) andalso (B == $>)) ->
  {error, {{Line, Column}, ?MODULE, {empty_name, A, B, Line}}};
scan_identifier(Line, Column, [A, B|_] = Rest, Acc)
    when ((A == $<) andalso (B == $<)); ((A == $>) andalso (B == $>))->
  {ok, Line, Column, lists:reverse(Acc), Rest};
scan_identifier(Line, Column, [H|T], Acc) when ?is_identifier(H) ->
  scan_identifier(Line, Column + 1, T, [H|Acc]);
scan_identifier(Line, Column, Rest, Acc) ->
  {ok, Line, Column, lists:reverse(Acc), Rest}.

%% Terminators

handle_terminator(Rest, Line, Column, Scope, Token, Tokens) ->
  case handle_terminator(Token, Scope) of
    {error, ErrorInfo} ->
      {error, ErrorInfo, atom_to_list(element(1, Token)) ++ Rest, Tokens};
    NewScope ->
      scan(Rest, Line, Column, NewScope, [Token|Tokens])
  end.
handle_terminator(_, #kapok_scanner_scope{check_terminators=false} = Scope) ->
  Scope;
handle_terminator(Token, #kapok_scanner_scope{terminators=Terminators} = Scope) ->
  case check_terminator(Token, Terminators) of
    {error, _} = Error -> Error;
    New -> Scope#kapok_scanner_scope{terminators=New}
  end.

check_terminator({O, _} = New, Terminators)
    when O == '('; O == '['; O == '{'; O == '%{'; O == '#{'; O == '<<' ->
  [New|Terminators];
check_terminator({C, _}, [{O, _}|Terminators])
    when O == '(',  C == ')';
         O == '[',  C == ']';
         O == '{',  C == '}';
         O == '%{', C == '}';
         O == '#{', C == '}';
         O == '<<', C == '>>' ->
  Terminators;
check_terminator({C, {Line, Column}}, [{Open, {{OpenLine, _}, _}}|_])
    when C == ')'; C == ']'; C == '}'; C == '>>' ->
  Close = terminator(Open),
  {error, {{Line, Column}, ?MODULE,
           {missing_container_terminator, atom_to_list(C), Open, OpenLine, Close}}};
check_terminator({C, {Line, Column}}, [])
    when C == ')'; C == ']'; C == '}'; C == '>>' ->
  {error, {{Line, Column}, ?MODULE, {unexpected_token, atom_to_list(C)}}};
check_terminator(_, Terminators) ->
  Terminators.

string_type([H|_T]) -> string_type(H);
string_type($") -> binary_string;
string_type($') -> list_string.

terminator('(') -> ')';
terminator('[') -> ']';
terminator('{') -> '}';
terminator('%{') -> '}';
terminator('#{') -> '}';
terminator('<<') -> '>>'.

%% Error

format_error({missing_terminator, Close, Open, OpenLine}) ->
  io_lib:format("missing terminator: ~ts (for \"~ts\" opening at line ~B)", [Close, Open, OpenLine]);
format_error({invalid_n_base_char, Char, Base, Line}) ->
  io_lib:format("invalid char: ~tc for ~B-base number at line ~B", [Char, Base, Line]);
format_error({invalid_eof, escape}) ->
  "invalid escape \\ at end of file";
format_error({missing_hex_sequence}) ->
  "missing hex sequence after \\x";
format_error({invalid_codepoint, CodePoint}) ->
  io_lib:format("invalid or reserved unicode codepoint ~tc", [CodePoint]);
format_error({empty_name, Char1, Char2, Line}) ->
  io_lib:format("empty name before ~tc~tc at line ~B", [Char1, Char2, Line]);
format_error({missing_container_terminator, Token, Open, OpenLine, Close}) ->
  Format = "unexpected token: \"~ts\". \"~ts\" starting at line ~B \
is missing terminator \"~ts\"",
  io_lib:format(Format, [Token, Open, OpenLine, Close]);
format_error({unexpected_token, Token}) ->
  io_lib:format("unexpected token: ~ts", [Token]);
format_error({invalid_space, Char, LineAfterChar}) ->
  io_lib:format("invalid space character U+~.16B before: ~ts", [Char, LineAfterChar]);
format_error({invalid_token, Line}) ->
  io_lib:format("invalid token: ~ts", [Line]).

