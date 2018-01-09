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
-define(is_single_quote(S), (S == $')).

%% Identifiers
-define(is_identifier_start(S),
        (?is_upcase(S) orelse ?is_downcase(S) orelse (S == $!) orelse (S == $$) orelse (S == $%) orelse (S == $*) orelse (S == $+) orelse (S == $-) orelse (S == $/) orelse (S == $<) orelse (S == $=) orelse (S == $>) orelse (S == $?) orelse (S == $@) orelse (S == $^) orelse (S == $_) orelse (S == $|))).
-define(is_identifier_char(S), (?is_identifier_start(S) orelse ?is_digit(S) orelse (S == $~) orelse (S == $&) orelse (S == $#))).

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
-define(is_keyword_or_atom(C), (C == 'keyword' orelse C == 'atom')).
-define(is_id(C), (C == 'identifier')).
-define(is_local_id(C), (?is_id(C) orelse ?is_keyword_or_atom(C))).
-define(is_dot(C), (C == 'dot')).
-define(is_id_or_dot(C), (?is_id(C) orelse ?is_dot(C))).
-define(is_local_id_or_dot(C), (?is_local_id(C) orelse ?is_dot(C))).
-define(is_list(C), (C == 'list' orelse C == 'literal_list')).
-define(is_cons_list(C), (C == 'cons_list')).
-define(is_parameter_list(C), (C == 'literal_list' orelse C == 'cons_list')).
-define(is_parameter_keyword(C), (C == 'keyword_optional' orelse C == 'keyword_rest' orelse C == 'keyword_key')).
-define(is_string(C), (C == 'list_string' orelse C == 'binary_string')).

-define(is_ns(Id), (Id == 'ns')).
-define(is_def_ns(Id), (Id == 'defns')).
-define(is_def_fn(Id), (Id == 'defn' orelse Id == 'defn-')).
-define(is_def_macro(Id), (Id == 'defmacro' orelse Id == 'defmacro-')).
-define(is_def_alias(Id), (Id == 'defalias' orelse Id == 'defalias-')).
-define(is_def(Id), (?is_def_fn(Id) orelse ?is_def_macro(Id) orelse ?is_def_alias(Id))).
-define(is_var_arg_op(Id), (Id == 'op-and' orelse Id == 'op-or' orelse Id == 'op-xor')).
-define(is_short_circuit_op(Id), (Id == 'op-andalso' orelse Id == 'op-orelse')).
-define(is_list_op(Id), (Id == 'op-++')).
-define(is_behaviour(Id), (Id == 'behavior' orelse Id == 'behaviour')).
-define(is_attribute(Id), (Id == 'attribute')).
-define(is_attr(Id), (?is_behaviour(Id) orelse ?is_attribute(Id))).
-define(is_special_form(Id), (Id == 'ns' orelse ?is_def(Id) orelse ?is_attr(Id))).

%% standdard libraries ns name
-define(STDLIB_NS, 'kapok').

%% default source file suffix
-define(SOURCE_FILE_SUFFIX, ".kpk").
-define(BEAM_FILE_SUFFIX, ".beam").
