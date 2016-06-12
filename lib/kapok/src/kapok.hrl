%% Scanner helper records and macros

-record(kapok_scanner_scope,
        {file,
         terminators=[],
         check_terminators=true,
         existing_atoms_only=false
        }).

%% Sign
-define(is_sign(S), (S == $+ orelse S == $-)).

%% Numbers
-define(is_hex(S), (?is_digit(S) orelse (S >= $A andalso S =< $F) orelse (S >= $a andalso S =< $f))).
-define(is_octal(S), (S >= $0 andalso S =< $7)).
-define(is_n_base(S, N), ((N =< 10 andalso (S >= $0 andalso S < ($0 + N))) orelse ?is_digit(S) orelse (S >= $A andalso S < ($A + N - 10)) orelse (S >= $a andalso S < ($a + N - 10)))).

%% Digits and letters
-define(is_digit(S), (S >= $0 andalso S =< $9)).
-define(is_upcase(S), (S >= $A andalso S =< $Z)).
-define(is_downcase(S), (S >= $a andalso S =< $z)).

%% Quotes
-define(is_quote(S), (S == $" orelse S == $')).

%% Identifiers
-define(is_identifier_start(S),
        (?is_upcase(S) orelse ?is_downcase(S) orelse (S == $!) orelse (S == $#) orelse (S == $$) orelse (S == $%) orelse (S == $*) orelse (S == $+) orelse (S == $-) orelse (S == $/) orelse (S == $<) orelse (S == $=) orelse (S == $>) orelse (S == $?) orelse (S == $@) orelse (S == $^) orelse (S == $_) orelse (S == $|))).
-define(is_identifier(S), (?is_identifier_start(S) orelse ?is_digit(S))).

%% Spaces
-define(is_horizontal_space(S), (S == $\s orelse S == $\t)).
-define(is_vertical_space(S), (S == $\r orelse S == $\n)).
-define(is_space(S), (?is_horizontal_space(S) orelse ?is_vertical_space(S))).
-define(is_invalid_space(S), (S == 16#A0)).


%% Other compiler helper macros

-define(line(Opts), kapok_utils:get_line(Opts)).
-define(m(M, K), maps:get(K, M)).

-define(is_op(T), (T == '+' orelse T == '-')).
-define(is_number(T), (T == 'number')).
-define(is_def(T), (T == 'defn' orelse T == 'defn-' orelse T == 'defmacro')).
-define(is_dot_id(T), (T == 'identifier' orelse T == 'dot')).
-define(is_local_id(T), (T == 'identifier' orelse T == 'atom')).
-define(is_list(T), (T == 'list' orelse T == 'literal_list')).
-define(is_cons_list(T), (T == 'cons_list')).
-define(is_arg_list(T), (?is_list(T) orelse ?is_cons_list(T))).
-define(is_string(T), (T == 'list_string' orelse T == 'binary_string')).

-define(is_behaviour(T), (T == 'behavior' orelse T == 'behaviour')).
-define(is_compile(T), (T == 'compile')).
-define(is_file(T), (T == 'file')).
-define(is_attribute(T), (T == 'attribute')).


%% default source file suffix
-define(SOURCE_FILE_SUFFIX, ".kpk").
-define(BEAM_FILE_SUFFIX, ".beam").


