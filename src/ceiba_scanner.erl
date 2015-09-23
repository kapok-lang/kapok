%%

-module(ceiba_scanner).
-include("ceiba.hrl").

-export([token_category/1,
         token_location/1,
         token_line/1,
         token_column/1,
         token_symbol/1,
         scan/3,
         scan/4]).

-type category() :: atom().
-type line() :: integer().
-type column() :: pos_integer().
-type location() :: line() | {line(),column()}.
-type symbol() :: atom() | float() | integer() | string() | binary().
-type attributes() :: location().
-type token() :: {category(), attributes(), symbol()}
               | {category(), attributes()}.
-type tokens() :: [token()].
-type option() :: {atom(), term()}.
-type options() :: [option()].
-type error_description() :: {string()}
                           | {string(), term()}
                           | {string(), string(), term()}.
-type error_info() :: {location(), error_description()}.

-spec token_category(Token :: token()) -> Return :: category().
token_category(Token) ->
    element(1, Token).
-spec token_location(Token :: token()) -> Return :: location().
token_location(Token) ->
    element(2, Token).
-spec token_line(Token :: token()) -> Return :: line().
token_line(Token) ->
    element(1, token_location(Token)).
-spec token_column(Token :: token()) -> Return :: column().
token_column(Token) ->
    element(2, token_location(Token)).
-spec token_symbol(Token :: token()) -> Return :: symbol().
token_symbol(Token) ->
    case Token of
        {_, _, Symbol} ->
            Symbol;
        {_, _} ->
            nil
    end.



%% start of scan()

-type tokens_result() :: {'ok', Tokens :: tokens(), EndLocation :: location()}
                       | {'error', ErrorInfo :: error_info(), Rest :: string(),
                          Tokens :: tokens()}.
-spec scan(String, Line, Options) -> Return when
      String :: string(),
      Line :: integer(),
      Options :: options(),
      Return :: tokens_result().
scan(String, Line, Options) ->
    scan(String, Line, 1, Options).

-spec scan(String, Line, Column, Options) -> Return when
      String :: string(),
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
    Context = #ceiba_scan_context{
                 file = File,
                 check_terminators = Check},
    scan(String, Line, Column, Context, []).

%% success
scan([], Line, Column, #ceiba_scan_context{terminators=[]}, Tokens) ->
    {ok, lists:reverse(Tokens), {Line, Column}};

%% terminator missing
scan([], EndLine, Column,
     #ceiba_scan_context{terminators=[{Open, {{OpenLine, _} ,_}}|_]},
     Tokens) ->
    Close = terminator(Open),
    Message = io_lib:format(
                "missing terminator: ~ts (for \"~ts\" opening at line ~B",
                [Close, Open, OpenLine]),
    {error, {{EndLine, Column}, {Message}}, [], lists:reverse(Tokens)};

%% Base integers

%% hex
scan([$0, $x, H|T], Line, Column, Context, Tokens) when ?is_hex(H) ->
    {Rest, Number, Length} = scan_hex([H|T], []),
    scan(Rest, Line, Column + 2 + Length, Context,
         [{number, {Line, Column}, Number}|Tokens]);
%% octal
scan([$0, H|T], Line, Column, Context, Tokens) when ?is_octal(H) ->
    {Rest, Number, Length} = scan_octal([H|T], []),
    scan(Rest, Line, Column + 1 + Length, Context,
         [{number, {Line, Column}, Number}|Tokens]);

%% flexible N(2 - 36) numeral bases
scan([B1, $r, H|T], Line, Column, Context, Tokens)
  when (B1 >= $2 andalso B1 =< $9) ->
    N = B1 - $0,
    case ?is_n_base(H, N) of
        true ->
            {Rest, Number, Length} = scan_n_base([H|T], N, []),
            scan(Rest, Line, Column + 2 + Length, Context,
                 [{number, {Line, Column}, Number}|Tokens]);
        _ ->
            Message = io_lib:format(
                        "invalid char: ~tc for ~B-base number at line ~B",
                        [H, N, Line]),
            {error, {{Line, Column}, {Message}}, [], Tokens}
    end;
scan([B1, B2, $r, H|T], Line, Column, Context, Tokens)
  when (B1 >= $1 andalso B1 =< $2), (B2 >= $0 andalso B2 =< $9);
       (B1 == $3), (B2 >= $0 andalso B2 =< $6) ->
    N = list_to_integer([B1, B2]),
    case ?is_n_base(H, N) of
        true ->
            {Rest, Number, Length} = scan_n_base([H|T], N, []),
            scan(Rest, Line, Column + 3 + Length, Context,
                 [{number, {Line, Column}, Number}|Tokens]);
        _ ->
            Message = io_lib:format(
                        "invalid char: ~tc for ~B-base number at line ~B",
                        [H, N, Line]),
            {error, {{Line, Column}, {Message}}, [], Tokens}
    end;

%% Comment

scan([$;|T], Line, Column, Context, Tokens) ->
    Rest = scan_comment(T),
    scan(Rest, Line, Column, Context, Tokens);

%% Char

scan([$\\, $x, ${,A,B,C,D,E,F,$}|T], Line, Column, Context, Tokens)
  when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E), ?is_hex(F) ->
    Char = escape_char([$\\, $x, ${,A,B,C,D,E,F,$}]),
    scan(T, Line, Column+10, Context, [{number, {Line, Column}, Char}|Tokens]);

scan([$\\, $x, ${,A,B,C,D,E,$}|T], Line, Column, Context, Tokens)
  when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E) ->
    Char = escape_char([$\\, $x, ${,A,B,C,D,E,$}]),
    scan(T, Line, Column+9, Context, [{number, {Line, Column}, Char}|Tokens]);

scan([$\\, $x, ${,A,B,C,D,$}|T], Line, Column, Context, Tokens)
  when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D) ->
    Char = escape_char([$\\, $x, ${,A,B,C,D,$}]),
    scan(T, Line, Column + 8, Context, [{number, {Line, Column}, Char}|Tokens]);

scan([$\\, $x, ${,A,B,C,$}|T], Line, Column, Context, Tokens)
  when ?is_hex(A), ?is_hex(B), ?is_hex(C) ->
    Char = escape_char([$\\, $x, ${,A,B,C,$}]),
    scan(T, Line, Column + 7, Context, [{number, {Line, Column}, Char}|Tokens]);

scan([$\\, $x, ${,A,B,$}|T], Line, Column, Context, Tokens)
  when ?is_hex(A), ?is_hex(B) ->
    Char = escape_char([$\\, $x, ${,A,B,$}]),
    scan(T, Line, Column + 6, Context, [{number, {Line, Column}, Char}|Tokens]);

scan([$\\, $x, ${,A,$}|T], Line, Column, Context, Tokens) when ?is_hex(A) ->
    Char = escape_char([$\\, $x, ${,A,$}]),
    scan(T, Line, Column + 5, Context, [{number, {Line, Column}, Char}|Tokens]);

scan([$\\, $x, A, B|T], Line, Column, Context, Tokens)
  when ?is_hex(A), ?is_hex(B) ->
    Char = escape_char([$\\, $x, A, B]),
    scan(T, Line, Column + 4, Context, [{number, {Line, Column}, Char}|Tokens]);

scan([$\\, $x, A|T], Line, Column, Context, Tokens) when ?is_hex(A) ->
    Char = escape_char([$\\, $x, A]),
    scan(T, Line, Column + 3, Context, [{number, {Line, Column}, Char}|Tokens]);

scan([$\\, $s,$p,$a,$c,$e|T], Line, Column, Context, Tokens) ->
    Char = $\s,
    scan(T, Line, Column + 6, Context, [{number, {Line, Column}, Char}|Tokens]);

scan([$\\, $t,$a,$b|T], Line, Column, Context, Tokens) ->
    Char = $\t,
    scan(T, Line, Column + 4, Context, [{number, {Line, Column}, Char}|Tokens]);

scan([$\\, $f,$o,$r,$m,$f,$e,$e,$d|T], Line, Column, Context, Tokens) ->
    Char = $\f,
    scan(T, Line, Column + 9, Context, [{number, {Line, Column}, Char}|Tokens]);

scan([$\\, $b,$a,$c,$k,$s,$p,$a,$c,$e|T], Line, Column, Context, Tokens) ->
    Char = $\b,
    scan(T, Line, Column+10, Context, [{number, {Line, Column}, Char}|Tokens]);

scan([$\\, $n,$e,$w,$l,$i,$n,$e|T], Line, Column, Context, Tokens) ->
    Char = $\n,
    scan(T, Line, Column + 8, Context, [{number, {Line, Column}, Char}|Tokens]);

scan([$\\, $r,$e,$t,$u,$r,$n|T], Line, Column, Context, Tokens) ->
    Char = $\r,
    scan(T, Line, Column + 7, Context, [{number, {Line, Column}, Char}|Tokens]);

%% End of line

scan("\\" = Original, Line, Column, _Context, Tokens) ->
    {error, {{Line, Column}, {"invalid escape \\ at end of file"}}, Original,
     Tokens};
scan("\\\n" = Original, Line, Column, _Context, Tokens) ->
    {error, {{Line, Column}, {"invalid escape \\ at end of file"}}, Original,
     Tokens};
scan("\\\r\n" = Original, Line, Column, _Context, Tokens) ->
    {error, {{Line, Column}, {"invalid escape \\ at end of file"}}, Original,
     Tokens};
scan("\\\n" ++ T, Line, _Column, Context, Tokens) ->
    scan(T, Line + 1, 1, Context, Tokens);
scan("\\\r\n" ++ T, Line, _Column, Context, Tokens) ->
    scan(T, Line + 1, 1, Context, Tokens);
scan("\n" ++ T, Line, _Column, Context, Tokens) ->
    scan(T, Line + 1, 1, Context, Tokens);
scan("\r\n" ++ T, Line, _Column, Context, Tokens) ->
    scan(T, Line + 1, 1, Context, Tokens);

%% Escaped chars

scan([$\\, H|T], Line, Column, Context, Tokens) ->
    Char = unescape_map(H),
    scan(T, Line, Column + 2, Context, [{number, {Line, Column}, Char}|Tokens]);

%% Strings

scan([$"|T], Line, Column, Context, Tokens) ->
    handle_string(T, Line, Column + 1, $", Context, Tokens);
scan([$'|T], Line, Column, Context, Tokens) ->
    handle_string(T, Line, Column + 1, $', Context, Tokens);

%% Atoms

scan([$:, H|T] = Original, Line, Column, Context, Tokens) when ?is_quote(H) ->
    case scan_string(Line, Column + 2, T, H) of
        {ok, NewLine, NewColumn, Bin, Rest} ->
            case unescape_tokens([Bin]) of
                {ok, Unescaped} ->
                    scan(Rest, NewLine, NewColumn, Context,
                         [{atom, {Line, Column}, Unescaped}|Tokens]);
                {error, ErrorDescription} ->
                    {error, {{Line, Column}, ErrorDescription}, Original,
                     Tokens}
            end;
        {error, ErrorInfo}->
            {error, ErrorInfo, Original, Tokens}
    end;
scan([$:, H|T], Line, Column, Context, Tokens) when ?is_identifier_start(H) ->
    handle_atom([H|T], Line, Column, Context, Tokens);

%% Containers

%% Binary
scan("<<" = Original, Line, Column, _Context, Tokens) ->
    {error, {{Line, Column}, {"invalid binary at end of file"}}, Original,
     Tokens};
scan([H, H|T], Line, Column, Context, Tokens) when H == $<; H == $> ->
    Token = {list_to_atom([H, H]), {Line, Column}},
    handle_terminator(T, Line, Column + 2, Context, Token, Tokens);

%% map, set
scan("#{" = Original, Line, Column, _Context, Tokens) ->
    {error, {{Line, Column}, {"invalid set at end of file"}}, Original, Tokens};
scan("%{" = Original, Line, Column, _Context, Tokens) ->
    {error, {{Line, Column}, {"invalid set at end of file"}}, Original, Tokens};
scan([H1, H2|T], Line, Column, Context, Tokens)
  when H1 == $#, H2 == ${; H1 == $%, H2 == ${ ->
    Token = {list_to_atom([H1, H2]), {Line, Column}},
    handle_terminator(T, Line, Column + 2, Context, Token, Tokens);

%% List, tuple, container seperator
scan([H|T], Line, Column, Context, Tokens)
  when H == $(; H == $); H == $[; H == $]; H == ${; H == $}; H == $, ->
    NextColumn = Column + 1,
    Token = {list_to_atom([H]), {Line, Column}},
    handle_terminator(T, Line, NextColumn, Context, Token, Tokens);

%% two chars operators

scan("~@" = Original, Line, Column, _Context, Tokens) ->
    {error, {{Line, Column}, {"invalid unquote-splicing at end of file"}},
     Original, Tokens};
scan([$~, $@|T], Line, Column, Context, Tokens) ->
    scan(T, Line, Column + 2, Context,
         [{unquote_splicing, {Line, Column}}|Tokens]);

%% one char operators

scan("`" = Original, Line, Column, _Context, Tokens) ->
    {error, {{Line, Column}, {"invalid backquote at end of file"}}, Original,
     Tokens};
scan([$`|T], Line, Column, Context, Tokens) ->
    scan(T, Line, Column + 1, Context, [{backquote, {Line, Column}}|Tokens]);

scan("'" = Original, Line, Column, _Context, Tokens) ->
    {error, {{Line, Column}, {"invalid quote at end of file"}}, Original,
     Tokens};
scan([$'|T], Line, Column, Context, Tokens) ->
    scan(T, Line, Column + 1, Context, [{quote, {Line, Column}}|Tokens]);

scan("~" = Original, Line, Column, _Context, Tokens) ->
    {error, {{Line, Column}, {"invalid unquote at end of file"}}, Original,
     Tokens};
scan([$~|T], Line, Column, Context, Tokens) ->
    scan(T, Line, Column + 1, Context, [{unquote, {Line, Column}}|Tokens]);

scan("&" = Original, Line, Column, _Context, Tokens) ->
    {error, {{Line, Column}, {"invalid rest at end of file"}}, Original,
     Tokens};
scan([$&|T], Line, Column, Context, Tokens) ->
    scan(T, Line, Column + 1, Context, [{rest, {Line, Column}}|Tokens]);

%% Integers and floats

scan([H|_] = Original, Line, Column, Context, Tokens) when ?is_digit(H) ->
    {Rest, Number, Length} = scan_number(Original, [], false),
    scan(Rest, Line, Column + Length, Context,
         [{number, {Line, Column}, Number}|Tokens]);

%% Identifiers

scan([H|_] = Original, Line, Column, Context, Tokens)
  when ?is_identifier_start(H) ->
    handle_identifier(Original, Line, Column, Context, Tokens);

%% Spaces
scan([H|T], Line, Column, Context, Tokens) when ?is_horizontal_space(H) ->
    scan(T, Line, Column + 1, Context, Tokens);

scan([H|T] = Original, Line, Column, _Context, Tokens)
  when ?is_invalid_space(H) ->
    Message = io_lib:format("invalid space character U+~.16B before: ", [H]),
    {error, {{Line, Column}, {Message, until_eol(T)}}, Original, Tokens};

scan(T, Line, Column, _Context, Tokens) ->
    {error, {{Line, Column}, {"invalid token: ", until_eol(T)}}, T, Tokens}.

%% end of scan()

until_eol(Rest) ->
    until_eol(Rest, []).
until_eol("\r\n" ++ _, Acc) -> lists:reverse(Acc);
until_eol("\n" ++ _, Acc)   -> lists:reverse(Acc);
until_eol([], Acc)          -> lists:reverse(Acc);
until_eol([H|T], Acc)       -> until_eol(T, [H|Acc]).

%% Integers and floats
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
    {Rest, list_to_float(lists:reverse(Acc)), length(Acc)};

%% Or integer.
scan_number(Rest, Acc, false) ->
    {Rest, list_to_integer(lists:reverse(Acc)), length(Acc)}.

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
    <<Char/utf8>> = unescape_chars(list_to_binary(List)),
    Char.

%% Unescape a series of tokens as returned by extract.
unescape_tokens(Tokens) ->
    unescape_tokens(Tokens, fun unescape_map/1, []).
unescape_tokens([], _Map, Acc) ->
    {ok, lists:reverse(Acc)};
unescape_tokens([Token|T], Map, Acc) ->
    case unescape_token(Token, Map) of
        {ok, Unescape} ->
            unescape_tokens(T, Map, [Unescape|Acc]);
        {error, ErrorDescription} ->
            {error, ErrorDescription}
    end.

unescape_token(Token, Map) when is_binary(Token) -> unescape_chars(Token, Map);
unescape_token(Other, _Map) -> {ok, Other}.

%% Unescape chars.
%% For instance, "\" "n" (two chars) needs to be converted to "\n" (one char).

unescape_chars(String) ->
    unescape_chars(String, fun unescape_map/1).
unescape_chars(String, Map) ->
    unescape_chars(String, Map, Map($x) == true, <<>>).
unescape_chars(<<$\\, $x, A, B, Rest/binary>>, Map, true, Acc)
  when ?is_hex(A), ?is_hex(B) ->
    append_escaped(Rest, Map, [A, B], true, Acc, 16);
unescape_chars(<<$\\, $x, A, Rest/binary>>, Map, true, Acc) when ?is_hex(A) ->
    append_escaped(Rest, Map, [A], true, Acc, 16);
unescape_chars(<<$\\, $x, ${,A,$}, Rest/binary>>, Map, true, Acc)
  when ?is_hex(A) ->
    append_escaped(Rest, Map, [A], true, Acc, 16);
unescape_chars(<<$\\, $x, ${,A,B,$}, Rest/binary>>, Map, true, Acc)
  when ?is_hex(A), ?is_hex(B) ->
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
    Message = "missing hex sequence after \\x",
    {error, {Message}};
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
            Message = io_lib:format("invalid or reserved unicode codepoint ~tc",
                                    [P]),
            {error, {Message}}
    end.

%% Unescape Helpers

unescape_map($0) -> 0;
unescape_map($a) -> 7;
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

handle_string(T, Line, Column, Term, Context,Tokens) ->
    case scan_string(Line, Column, T, Term) of
        {ok, NewLine, NewColumn, Bin, Rest} ->
            Unescaped = unescape_tokens([Bin]),
            scan(Rest, NewLine, NewColumn, Context,
                 [{string_type(Term), {Line, Column-1}, Unescaped}|Tokens]);
        {error, ErrorInfo} ->
            {error, ErrorInfo, T, Tokens}
    end.

scan_string(Line, Column, T, Term) ->
    scan_string(Line, Column, T, Term, []).
scan_string(Line, Column, [], Term, Acc) ->
    {error,
     {{Line, Column}, {io_lib:format("missing terminator: ~ts", [[Term]]),
                       lists:reverse(Acc)}}};
%% Terminators
scan_string(Line, Column, [Term|Remaining], Term, Acc) ->
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
    scan_string(Line+1, 1, Rest, Term, [$\n |Acc]);
scan_string(Line, Column, [$\\, Term|Rest], Term, Acc) ->
    scan_string(Line, Column+2, Rest, Term, [Term|Acc]);
scan_string(Line, Column, [$\\, Char|Rest], Term, Acc) ->
    scan_string(Line, Column+2, Rest, Term, [Char, $\\|Acc]);
%% Catch all clause
scan_string(Line, Column, [Char|Rest], Term, Acc) ->
    scan_string(Line, Column+1, Rest, Term, [Char|Acc]).

%% Identifiers

handle_atom(T, Line, Column, Context, Tokens) ->
    case scan_identifier(Line, Column, T) of
        {ok, NewLine, NewColumn, Identifier, Rest} ->
            scan(Rest, NewLine, NewColumn, Context,
                 [{atom, {Line, Column}, list_to_binary(Identifier)}|Tokens]);
        {error, ErrorInfo} ->
            {error, ErrorInfo, [$: | T], Tokens}
    end.

handle_identifier(T, Line, Column, Context, Tokens) ->
    case scan_identifier(Line, Column, T) of
        {ok, NewLine, NewColumn, Identifier, Rest} ->
            scan(Rest, NewLine, NewColumn, Context,
                 [{identifier, {Line, Column}, Identifier}|Tokens]);
        {error, ErrorInfo} ->
            {error, ErrorInfo, T, Tokens}
    end.

scan_identifier(Line, Column, T) ->
    scan_identifier(Line, Column, T, []).
scan_identifier(Line, Column, [], Acc) ->
    {ok, Line, Column, lists:reverse(Acc), []};
%% binary terminators "<<" ">>" are always higher priority
scan_identifier(Line, Column, [A, B|_], [])
  when ((A == $<) andalso (B == $<)); ((A == $>) andalso (B == $>)) ->
    {error, {{Line, Column}, {io_lib:format("empty name before ~tc~tc", [A, B])}}};
scan_identifier(Line, Column, [A, B|_] = Rest, Acc)
  when ((A == $<) andalso (B == $<)); ((A == $>) andalso (B == $>))->
    {ok, Line, Column, lists:reverse(Acc), Rest};
scan_identifier(Line, Column, [H|T], Acc) when ?is_identifier(H) ->
    scan_identifier(Line, Column + 1, T, [H|Acc]);
scan_identifier(Line, Column, Rest, Acc) ->
    {ok, Line, Column, lists:reverse(Acc), Rest}.

%% Terminators

handle_terminator(Rest, Line, Column, Context, Token, Tokens) ->
    case handle_terminator(Token, Context) of
        {error, ErrorInfo} ->
            {error, ErrorInfo, atom_to_list(element(1, Token)) ++ Rest, Tokens};
        NewContext ->
            scan(Rest, Line, Column, NewContext, [Token|Tokens])
    end.
handle_terminator(_, #ceiba_scan_context{check_terminators=false} = Context) ->
    Context;
handle_terminator(Token,
                  #ceiba_scan_context{terminators=Terminators} = Context) ->
    case check_terminator(Token, Terminators) of
        {error, _} = Error -> Error;
        New -> Context#ceiba_scan_context{terminators=New}
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
    MessagePrefix = "unexpected token: \"",
    Format = "\". \"~ts\" starting at line ~B is missing terminator \"~ts\"",
    MessageSuffix = io_lib:format(Format, [Open, OpenLine, Close]),
    {error, {{Line, Column}, {MessagePrefix, MessageSuffix, atom_to_list(C)}}};
check_terminator({C, {Line, Column}}, [])
  when C == ')'; C == ']'; C == '}'; C == '>>' ->
    {error, {{Line, Column}, {"unexpected token: ", atom_to_list(C)}}};
check_terminator(_, Terminators) ->
    Terminators.

string_type($") -> binary_string;
string_type($') -> list_string.

terminator('(') -> ')';
terminator('[') -> ']';
terminator('{') -> '}';
terminator('%{') -> '}';
terminator('#{') -> '}';
terminator('<<') -> '>>'.

