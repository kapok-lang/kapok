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

inline(?core, 'is-atom', 1, 'normal') -> {erlang, is_atom, 1, 'normal'};
inline(?core, 'is-binary', 1, 'normal') -> {erlang, is_binary, 1, 'normal'};
inline(?core, 'is_bitstring', 1, 'normal') -> {erlang, is_bitstring, 1, 'normal'};
inline(?core, 'is-boolean', 1, 'normal') -> {erlang, is_boolean, 1, 'normal'};
inline(?core, 'is-builtin', 3, 'normal') -> {erlang, is_builtin, 3, 'normal'};
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

inline(M, F, A, P) ->
  %% TODO add impl
  io:format("****** call inline, M: ~p, F: ~p, A: ~p, P: ~p~n", [M, F, A, P]),
  false.

