%%
-define(m(M, K), maps:get(K, M)).
-define(line(Opts), kapok_utils:get_line(Opts)).

-record(kapok_scanner_scope,
        {file,
         terminators=[],
         check_terminators=true,
         existing_atoms_only=false
        }).

-record(kapok_scope,
        {module = nil,
         file = nil,
         function = nil,
         context = nil,
         vars = [],
         export_vars = nil,
         super = false,
         caller = false,
         return = true
        }).

%% Sign
-define(is_sign(S), ((S == $+) orelse (S == $-))).

%% Numbers
-define(is_hex(S), (?is_digit(S) orelse (S >= $A andalso S =< $F) orelse (S >= $a andalso S =< $f))).
-define(is_octal(S), (S >= $0 andalso S =< $7)).
-define(is_n_base(S, N), ((N =< 10 andalso (S >= $0 andalso S < ($0 + N))) orelse ?is_digit(S) orelse (S >= $A andalso S < ($A + N - 10)) orelse (S >= $a andalso S < ($a + N - 10)))).

%% Digits and letters
-define(is_digit(S), (S >= $0 andalso S =< $9)).
-define(is_upcase(S), (S >= $A andalso S =< $Z)).
-define(is_downcase(S), (S >= $a andalso S =< $z)).

%% Identifiers
-define(is_identifier_start(S),
        (?is_upcase(S) orelse ?is_downcase(S) orelse (S == $!) orelse (S == $#) orelse (S == $$) orelse (S == $*) orelse (S == $/) orelse (S == $<) orelse (S == $=) orelse (S == $>) orelse (S == $?) orelse (S == $@) orelse (S == $^) orelse (S == $_) orelse (S == $|))).
-define(is_identifier(S), (?is_identifier_start(S) orelse ?is_digit(S) orelse ?is_sign(S))).

%% Quotes
-define(is_quote(S), (S == $" orelse S == $')).

%% Spaces
-define(is_horizontal_space(S), ((S == $\s) orelse (S == $\t))).
-define(is_vertical_space(S), ((S == $\r) orelse (S == $\n))).
-define(is_space(S), (?is_horizontal_space(S) orelse ?is_vertical_space(S))).
-define(is_invalid_space(S), (S == 16#A0)).

