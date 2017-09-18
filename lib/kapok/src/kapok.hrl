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
        (?is_upcase(S) orelse ?is_downcase(S) orelse (S == $!) orelse (S == $$) orelse (S == $%) orelse (S == $*) orelse (S == $+) orelse (S == $-) orelse (S == $/) orelse (S == $<) orelse (S == $=) orelse (S == $>) orelse (S == $?) orelse (S == $@) orelse (S == $_) orelse (S == $|))).
-define(is_identifier(S), (?is_identifier_start(S) orelse ?is_digit(S) orelse (S == $~) orelse (S == $&) orelse (S == $#) orelse (S == $^))).

%% Spaces
-define(is_horizontal_space(S), (S == $\s orelse S == $\t)).
-define(is_vertical_space(S), (S == $\r orelse S == $\n)).
-define(is_space(S), (?is_horizontal_space(S) orelse ?is_vertical_space(S))).
-define(is_invalid_space(S), (S == 16#A0)).


%% Other compiler helper macros

-define(line(Opts), kapok_utils:meta_line(Opts)).
-define(m(M, K), maps:get(K, M)).

-define(is_op(C), (C == '+' orelse C == '-')).
-define(is_number(C), (C == 'number')).
-define(is_dot_id(C), (C == 'identifier' orelse C == 'dot')).
-define(is_local_id(C), (C == 'identifier' orelse C == 'atom')).
-define(is_list(C), (C == 'list' orelse C == 'literal_list')).
-define(is_cons_list(C), (C == 'cons_list')).
-define(is_parameter_list(C), (C == 'literal_list' orelse C == 'cons_list')).
-define(is_parameter_keyword(C), (C == 'keyword_optional' orelse C == 'keyword_rest' orelse C == 'keyword_key')).
-define(is_string(C), (C == 'list_string' orelse C == 'binary_string')).

-define(is_def_ns(Id), (Id == 'defns')).
-define(is_def_fn(Id), (Id == 'defn' orelse Id == 'defn-')).
-define(is_def_macro(Id), (Id == 'defmacro')).
-define(is_def_alias(Id), (Id == 'defalias' orelse Id == 'defalias-')).
-define(is_def(Id), (?is_def_fn(Id) orelse ?is_def_macro(Id) orelse ?is_def_alias(Id))).
-define(is_behaviour(Id), (Id == 'behavior' orelse Id == 'behaviour')).
-define(is_compile(Id), (Id == 'compile')).
-define(is_file(Id), (Id == 'file')).
-define(is_attribute(Id), (Id == 'attribute')).
-define(is_attr(Id), (?is_behaviour(Id) orelse ?is_compile(Id) orelse ?is_file(Id) orelse ?is_attribute(Id))).
-define(is_special_form(Id), (Id == 'ns' orelse ?is_def(Id) orelse ?is_attr(Id))).

%% default source file suffix
-define(SOURCE_FILE_SUFFIX, ".kpk").
-define(BEAM_FILE_SUFFIX, ".beam").
