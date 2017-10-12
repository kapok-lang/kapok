%% Rewrite local/remote call when translating AST to Erlang Abstract Format
-module(kapok_rewrite).
-export([inline/4]).

%% Convenience variables

-define(inline_nfun(M, F, A), inline(M, F, A, 'normal')).
-define(nfun(M, F, A), {M, F, A, 'normal'}).

%% Inline

%% Inlines functions means to transform function calls to their erlang module equivalent.
%% Rules for inlining are straight-forward, the same arugment arity and order are kept.
%% There are two main reasons to inline these functions:
%% 1. Efficiency. To avoid another function call.
%% 2. Syntax Limit. Only a limited predicates and built-in functions are allowed in guards.

?inline_nfun('atom', 'to-char-list', 1) -> ?nfun(erlang, atom_to_list, 1);

%% guard predicates
?inline_nfun('core', 'atom?', 1) -> ?nfun(erlang, is_atom, 1);
?inline_nfun('core', 'binary?', 1) -> ?nfun(erlang, is_binary, 1);
?inline_nfun('core', 'bitstring?', 1) -> ?nfun(erlang, is_bitstring, 1);
?inline_nfun('core', 'boolean?', 1) -> ?nfun(erlang, is_boolean, 1);
?inline_nfun('core', 'float?', 1) -> ?nfun(erlang, is_float, 1);
?inline_nfun('core', 'function?', 1) -> ?nfun(erlang, is_function, 1);
?inline_nfun('core', 'function?', 2) -> ?nfun(erlang, is_function, 2);
?inline_nfun('core', 'integer?', 1) -> ?nfun(erlang, is_integer, 1);
?inline_nfun('core', 'list?', 1) -> ?nfun(erlang, is_list, 1);
?inline_nfun('core', 'map?', 1) -> ?nfun(erlang, is_map, 1);
?inline_nfun('core', 'number?', 1) -> ?nfun(erlang, is_number, 1);
?inline_nfun('core', 'pid?', 1) -> ?nfun(erlang, is_pid, 1);
?inline_nfun('core', 'pmod?', 1) -> ?nfun(erlang, is_pmod, 1);
?inline_nfun('core', 'port?', 1) -> ?nfun(erlang, is_port, 1);
?inline_nfun('core', 'reference?', 1) -> ?nfun(erlang, is_reference, 1);
?inline_nfun('core', 'tuple?', 1) -> ?nfun(erlang, is_tuple, 1);

%% guard built-in functions
?inline_nfun('core', 'abs', 1) -> ?nfun(erlang, abs, 1);
?inline_nfun('core', 'bit-size', 1) -> ?nfun(erlang, bit_size, 1);
?inline_nfun('core', 'byte-size', 1) -> ?nfun(erlang, byte_size, 1);
?inline_nfun('core', 'elem', 2) -> ?nfun(erlang, element, 2);
?inline_nfun('core', 'number-to-float', 1) -> ?nfun(erlang, float, 1);
?inline_nfun('core', 'hd', 1) -> ?nfun(erlang, hd, 1);
?inline_nfun('core', 'head', 1) -> ?nfun(erlang, hd, 1);
?inline_nfun('core', 'length', 1) -> ?nfun(erlang, length, 1);
?inline_nfun('core', 'node', 0) -> ?nfun(erlang, node, 0);
?inline_nfun('core', 'node', 1) -> ?nfun(erlang, node, 1);
?inline_nfun('core', 'round', 1) -> ?nfun(erlang, round, 1);
?inline_nfun('core', 'self', 0) -> ?nfun(erlang, self, 0);
?inline_nfun('core', 'tail', 1) -> ?nfun(erlang, tl, 1);
?inline_nfun('core', 'tl', 1) -> ?nfun(erlang, tl, 1);
?inline_nfun('core', 'trunc', 1) -> ?nfun(erlang, trunc, 1);
?inline_nfun('core', 'tuple-size', 1) -> ?nfun(erlang, tuple_size, 1);

%% term comparators
?inline_nfun('core', '<', 2) -> ?nfun(erlang, '<', 2);
?inline_nfun('core', '>', 2) -> ?nfun(erlang, '>', 2);
?inline_nfun('core', '<=', 2) -> ?nfun(erlang, '<=', 2);
?inline_nfun('core', '>=', 2) -> ?nfun(erlang, '>=', 2);
?inline_nfun('core', '==', 2) -> ?nfun(erlang, '==', 2);
?inline_nfun('core', '!=', 2) -> ?nfun(erlang, '/=', 2);
?inline_nfun('core', '===', 2) -> ?nfun(erlang, '=:=', 2);
?inline_nfun('core', '!==', 2) -> ?nfun(erlang, '=/=', 2);

%% arithmetic operators
%% unary +/- is handled by the parser, we just handle binary +/- here.
?inline_nfun('core', '+', 2) -> ?nfun(erlang, '+', 2);
?inline_nfun('core', '-', 2) -> ?nfun(erlang, '-', 2);
?inline_nfun('core', '*', 2) -> ?nfun(erlang, '*', 2);
?inline_nfun('core', '/', 2) -> ?nfun(erlang, '/', 2);
?inline_nfun('core', 'div', 2) -> ?nfun(erlang, 'div', 2);
?inline_nfun('core', 'rem', 2) -> ?nfun(erlang, 'rem', 2);
?inline_nfun('core', 'bit-not', 1) -> ?nfun(erlang, 'bnot', 1);
?inline_nfun('core', 'bnot', 1) -> ?nfun(erlang, 'bnot', 1);
?inline_nfun('core', 'bit-and', 2) -> ?nfun(erlang, 'band', 2);
?inline_nfun('core', 'band', 2) -> ?nfun(erlang, 'band', 2);
?inline_nfun('core', 'bit-or', 2) -> ?nfun(erlang, 'bor', 2);
?inline_nfun('core', 'bor', 2) -> ?nfun(erlang, 'bor', 2);
?inline_nfun('core', 'bit-xor', 2) -> ?nfun(erlang, 'bxor', 2);
?inline_nfun('core', 'bxor', 2) -> ?nfun(erlang, 'bxor', 2);
?inline_nfun('core', 'bit-shift-left', 2) -> ?nfun(erlang, 'bsl', 2);
?inline_nfun('core', 'bsl', 2) -> ?nfun(erlang, 'bsl', 2);
?inline_nfun('core', 'bit-shift-right', 2) -> ?nfun(erlang, 'bsr', 2);
?inline_nfun('core', 'bsr', 2) -> ?nfun(erlang, 'bsr', 2);

%% boolean operators
?inline_nfun('core', 'xor', 2) -> ?nfun(erlang, 'xor', 2);

inline(_M, _F, _A, _P) ->
  %% TODO add impl
  false.


%% rewrite core.set-elem/3, index + 1
