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

inline(?atom, 'to-char-list', 1, 'normal') ->
  io:format("****** call inline, M: ~p, F: ~p, A: ~p, P: ~p~n", [?atom, 'to-char-list', 1, 'normal']),
  {erlang, atom_to_list, 1, 'normal'};

inline(M, F, A, P) ->
  io:format("****** call inline, M: ~p, F: ~p, A: ~p, P: ~p~n", [M, F, A, P]),
  false.

