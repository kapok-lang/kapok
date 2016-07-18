%% Rewrite local/remote call when translating AST to Erlang Abstract Format
-module(kapok_rewrite).
-export([inline/4]).

%% Convenience variables

-define(atom, 'kapok.atom').
-define(core, 'kapok.core').

%% Inline

%% Inlines functions means to transform function calls to their erlang module equivalent.
%% Rules for inlining are straight-forward, the same arugment arity and order are kept.
%% There are two main reasons to inline these functions:
%% 1. Efficiency. To avoid another function call.
%% 2. Syntax Limit. Only a limited predicates and built-in functions are allowed in guards.

inline(?atom, 'to-char-list', 1, 'normal') -> {erlang, atom_to_list, 1, 'normal'};

%% guard predicates
inline(?core, 'is-atom', 1, 'normal') -> {erlang, is_atom, 1, 'normal'};
inline(?core, 'is-binary', 1, 'normal') -> {erlang, is_binary, 1, 'normal'};
inline(?core, 'is_bitstring', 1, 'normal') -> {erlang, is_bitstring, 1, 'normal'};
inline(?core, 'is-boolean', 1, 'normal') -> {erlang, is_boolean, 1, 'normal'};
inline(?core, 'is-float', 1, 'normal') -> {erlang, is_float, 1, 'normal'};
inline(?core, 'is-function', 1, 'normal') -> {erlang, is_function, 1, 'normal'};
inline(?core, 'is-function', 2, 'normal') -> {erlang, is_function, 2, 'normal'};
inline(?core, 'is-integer', 1, 'normal') -> {erlang, is_integer, 1, 'normal'};
inline(?core, 'is-list', 1, 'normal') -> {erlang, is_list, 1, 'normal'};
inline(?core, 'is-map', 1, 'normal') -> {erlang, is_map, 1, 'normal'};
inline(?core, 'is-number', 1, 'normal') -> {erlang, is_number, 1, 'normal'};
inline(?core, 'is-pid', 1, 'normal') -> {erlang, is_pid, 1, 'normal'};
inline(?core, 'is-pmod', 1, 'normal') -> {erlang, is_pmod, 1, 'normal'};
inline(?core, 'is-port', 1, 'normal') -> {erlang, is_port, 1, 'normal'};
inline(?core, 'is-reference', 1, 'normal') -> {erlang, is_reference, 1, 'normal'};
inline(?core, 'is-tuple', 1, 'normal') -> {erlang, is_tuple, 1, 'normal'};

%% guard built-in functions
inline(?core, 'abs', 1, 'normal') -> {erlang, abs, 1, 'normal'};
inline(?core, 'bit-size', 1, 'normal') -> {erlang, bit_size, 1, 'normal'};
inline(?core, 'byte-size', 1, 'normal') -> {erlang, byte_size, 1, 'normal'};
inline(?core, 'elem', 2, 'normal') -> {erlang, element, 2, 'normal'};
inline(?core, 'number-to-float', 1, 'normal') -> {erlang, float, 1, 'normal'};
inline(?core, 'hd', 1, 'normal') -> {erlang, hd, 1, 'normal'};
inline(?core, 'head', 1, 'normal') -> {erlang, hd, 1, 'normal'};
inline(?core, 'length', 1, 'normal') -> {erlang, length, 1, 'normal'};
inline(?core, 'node', 0, 'normal') -> {erlang, node, 0, 'normal'};
inline(?core, 'node', 1, 'normal') -> {erlang, node, 1, 'normal'};
inline(?core, 'round', 1, 'normal') -> {erlang, round, 1, 'normal'};
inline(?core, 'self', 0, 'normal') -> {erlang, self, 0, 'normal'};
inline(?core, 'tail', 1, 'normal') -> {erlang, tl, 1, 'normal'};
inline(?core, 'tl', 1, 'normal') -> {erlang, tl, 1, 'normal'};
inline(?core, 'trunc', 1, 'normal') -> {erlang, trunc, 1, 'normal'};
inline(?core, 'tuple-size', 1, 'normal') -> {erlang, tuple_size, 1, 'normal'};

%% term comparators
inline(?core, '<', 2, 'normal') -> {erlang, '<', 2, 'normal'};
inline(?core, '>', 2, 'normal') -> {erlang, '>', 2, 'normal'};
inline(?core, '<=', 2, 'normal') -> {erlang, '<=', 2, 'normal'};
inline(?core, '>=', 2, 'normal') -> {erlang, '>=', 2, 'normal'};
inline(?core, '==', 2, 'normal') -> {erlang, '==', 2, 'normal'};
inline(?core, '!=', 2, 'normal') -> {erlang, '/=', 2, 'normal'};
inline(?core, '===', 2, 'normal') -> {erlang, '=:=', 2, 'normal'};
inline(?core, '!==', 2, 'normal') -> {erlang, '=/=', 2, 'normal'};

%% arithmetic operators
%% unary +/- is handled by the parser, we just handle binary +/- here.
inline(?core, '+', 2, 'normal') -> {erlang, '+', 2, 'normal'};
inline(?core, '-', 2, 'normal') -> {erlang, '-', 2, 'normal'};
inline(?core, '*', 2, 'normal') -> {erlang, '*', 2, 'normal'};
inline(?core, '/', 2, 'normal') -> {erlang, '/', 2, 'normal'};
inline(?core, 'div', 2, 'normal') -> {erlang, 'div', 2, 'normal'};
inline(?core, 'rem', 2, 'normal') -> {erlang, 'rem', 2, 'normal'};
inline(?core, 'bit-not', 1, 'normal') -> {erlang, 'bnot', 1, 'normal'};
inline(?core, 'bnot', 1, 'normal') -> {erlang, 'bnot', 1, 'normal'};
inline(?core, 'bit-and', 2, 'normal') -> {erlang, 'band', 2, 'normal'};
inline(?core, 'band', 2, 'normal') -> {erlang, 'band', 2, 'normal'};
inline(?core, 'bit-or', 2, 'normal') -> {erlang, 'bor', 2, 'normal'};
inline(?core, 'bor', 2, 'normal') -> {erlang, 'bor', 2, 'normal'};
inline(?core, 'bit-xor', 2, 'normal') -> {erlang, 'bxor', 2, 'normal'};
inline(?core, 'bxor', 2, 'normal') -> {erlang, 'bxor', 2, 'normal'};
inline(?core, 'bit-shift-left', 2, 'normal') -> {erlang, 'bsl', 2, 'normal'};
inline(?core, 'bsl', 2, 'normal') -> {erlang, 'bsl', 2, 'normal'};
inline(?core, 'bit-shift-right', 2, 'normal') -> {erlang, 'bsr', 2, 'normal'};
inline(?core, 'bsr', 2, 'normal') -> {erlang, 'bsr', 2, 'normal'};

%% boolean operators
%% TODO change them to the macro versions, and remove them from the inlines
inline(?core, 'not', 1, 'normal') -> {erlang, 'not', 1, 'normal'};
inline(?core, 'and', 2, 'normal') -> {erlang, 'andalso', 2, 'normal'};
inline(?core, 'or', 2, 'normal') -> {erlang, 'orelse', 2, 'normal'};
inline(?core, 'xor', 2, 'normal') -> {erlang, 'xor', 2, 'normal'};

inline(M, F, A, P) ->
  %% TODO add impl
  io:format("****** call inline, M: ~p, F: ~p, A: ~p, P: ~p~n", [M, F, A, P]),
  false.


%% rewrite core.set-elem/3, index + 1
