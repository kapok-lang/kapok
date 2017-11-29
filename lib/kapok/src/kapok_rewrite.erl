%% Rewrite local/remote call when translating AST to Erlang Abstract Format
-module(kapok_rewrite).
-export([rewrite/7, inline/4]).

%% Convenience variables

-define(inline_nfun(M, F, A), inline(M, F, A, 'normal')).
-define(nfun(M, F, A), {M, F, A, 'normal'}).

%% Inline

%% Inlines functions means to transform function calls to their erlang module equivalent.
%% Rules for inlining are straight-forward, the same arugment arity and order are kept.
%% There are two main reasons to inline these functions:
%% 1. Efficiency. To avoid another function call.
%% 2. Syntax Limit. Only a limited predicates and built-in functions are allowed in guards.

?inline_nfun('kapok.atom', 'to-char-list', 1) -> ?nfun(erlang, atom_to_list, 1);

%% guard predicates
?inline_nfun('kapok.core', 'atom?', 1) -> ?nfun(erlang, is_atom, 1);
?inline_nfun('kapok.core', 'binary?', 1) -> ?nfun(erlang, is_binary, 1);
?inline_nfun('kapok.core', 'bitstring?', 1) -> ?nfun(erlang, is_bitstring, 1);
?inline_nfun('kapok.core', 'boolean?', 1) -> ?nfun(erlang, is_boolean, 1);
?inline_nfun('kapok.core', 'float?', 1) -> ?nfun(erlang, is_float, 1);
?inline_nfun('kapok.core', 'function?', 1) -> ?nfun(erlang, is_function, 1);
?inline_nfun('kapok.core', 'function?', 2) -> ?nfun(erlang, is_function, 2);
?inline_nfun('kapok.core', 'integer?', 1) -> ?nfun(erlang, is_integer, 1);
?inline_nfun('kapok.core', 'list?', 1) -> ?nfun(erlang, is_list, 1);
?inline_nfun('kapok.core', 'map?', 1) -> ?nfun(erlang, is_map, 1);
?inline_nfun('kapok.core', 'number?', 1) -> ?nfun(erlang, is_number, 1);
?inline_nfun('kapok.core', 'pid?', 1) -> ?nfun(erlang, is_pid, 1);
?inline_nfun('kapok.core', 'pmod?', 1) -> ?nfun(erlang, is_pmod, 1);
?inline_nfun('kapok.core', 'port?', 1) -> ?nfun(erlang, is_port, 1);
?inline_nfun('kapok.core', 'reference?', 1) -> ?nfun(erlang, is_reference, 1);
?inline_nfun('kapok.core', 'tuple?', 1) -> ?nfun(erlang, is_tuple, 1);

%% guard built-in functions
?inline_nfun('kapok.core', 'abs', 1) -> ?nfun(erlang, abs, 1);
?inline_nfun('kapok.core', 'bit-size', 1) -> ?nfun(erlang, bit_size, 1);
?inline_nfun('kapok.core', 'byte-size', 1) -> ?nfun(erlang, byte_size, 1);
?inline_nfun('kapok.core', 'number-to-float', 1) -> ?nfun(erlang, float, 1);
?inline_nfun('kapok.core', 'hd', 1) -> ?nfun(erlang, hd, 1);
?inline_nfun('kapok.core', 'head', 1) -> ?nfun(erlang, hd, 1);
?inline_nfun('kapok.core', 'length', 1) -> ?nfun(erlang, length, 1);
?inline_nfun('kapok.core', 'node', 0) -> ?nfun(erlang, node, 0);
?inline_nfun('kapok.core', 'node', 1) -> ?nfun(erlang, node, 1);
?inline_nfun('kapok.core', 'round', 1) -> ?nfun(erlang, round, 1);
?inline_nfun('kapok.core', 'self', 0) -> ?nfun(erlang, self, 0);
?inline_nfun('kapok.core', 'tail', 1) -> ?nfun(erlang, tl, 1);
?inline_nfun('kapok.core', 'tl', 1) -> ?nfun(erlang, tl, 1);
?inline_nfun('kapok.core', 'trunc', 1) -> ?nfun(erlang, trunc, 1);
?inline_nfun('kapok.core', 'tuple-size', 1) -> ?nfun(erlang, tuple_size, 1);

%% term comparators
?inline_nfun('kapok.core', '<', 2) -> ?nfun(erlang, '<', 2);
?inline_nfun('kapok.core', '>', 2) -> ?nfun(erlang, '>', 2);
?inline_nfun('kapok.core', '<=', 2) -> ?nfun(erlang, '=<', 2);
?inline_nfun('kapok.core', '>=', 2) -> ?nfun(erlang, '>=', 2);
?inline_nfun('kapok.core', '==', 2) -> ?nfun(erlang, '==', 2);
?inline_nfun('kapok.core', '!=', 2) -> ?nfun(erlang, '/=', 2);
?inline_nfun('kapok.core', '===', 2) -> ?nfun(erlang, '=:=', 2);
?inline_nfun('kapok.core', '!==', 2) -> ?nfun(erlang, '=/=', 2);

%% arithmetic operators
%% unary +/- is handled by the parser, we just handle binary +/- here.
?inline_nfun('kapok.core', '+', 2) -> ?nfun(erlang, '+', 2);
?inline_nfun('kapok.core', '-', 2) -> ?nfun(erlang, '-', 2);
?inline_nfun('kapok.core', '*', 2) -> ?nfun(erlang, '*', 2);
?inline_nfun('kapok.core', '/', 2) -> ?nfun(erlang, '/', 2);
?inline_nfun('kapok.core', 'div', 2) -> ?nfun(erlang, 'div', 2);
?inline_nfun('kapok.core', 'rem', 2) -> ?nfun(erlang, 'rem', 2);
?inline_nfun('kapok.core', 'bit-not', 1) -> ?nfun(erlang, 'bnot', 1);
?inline_nfun('kapok.core', 'bnot', 1) -> ?nfun(erlang, 'bnot', 1);
?inline_nfun('kapok.core', 'bit-and', 2) -> ?nfun(erlang, 'band', 2);
?inline_nfun('kapok.core', 'band', 2) -> ?nfun(erlang, 'band', 2);
?inline_nfun('kapok.core', 'bit-or', 2) -> ?nfun(erlang, 'bor', 2);
?inline_nfun('kapok.core', 'bor', 2) -> ?nfun(erlang, 'bor', 2);
?inline_nfun('kapok.core', 'bit-xor', 2) -> ?nfun(erlang, 'bxor', 2);
?inline_nfun('kapok.core', 'bxor', 2) -> ?nfun(erlang, 'bxor', 2);
?inline_nfun('kapok.core', 'bit-shift-left', 2) -> ?nfun(erlang, 'bsl', 2);
?inline_nfun('kapok.core', 'bsl', 2) -> ?nfun(erlang, 'bsl', 2);
?inline_nfun('kapok.core', 'bit-shift-right', 2) -> ?nfun(erlang, 'bsr', 2);
?inline_nfun('kapok.core', 'bsr', 2) -> ?nfun(erlang, 'bsr', 2);

%% boolean operators
?inline_nfun('kapok.core', 'xor', 2) -> ?nfun(erlang, 'xor', 2);

%% integer
?inline_nfun('kapok.integer', 'to-string', 1) -> ?nfun(erlang, integer_to_binary, 1);
?inline_nfun('kapok.integer', 'to-string', 2) -> ?nfun(erlang, integer_to_binary, 2);
?inline_nfun('kapok.integer', 'to-char-list', 1) -> ?nfun(erlang, integer_to_list, 1);
?inline_nfun('kapok.integer', 'to-char-list', 2) -> ?nfun(erlang, integer_to_list, 2);

%% list
?inline_nfun('kapok.list', 'to-atom', 1) -> ?nfun(erlang, list_to_atom, 1);
?inline_nfun('kapok.list', 'to-existing-atom', 1) -> ?nfun(erlang, list_to_existing_atom, 1);
?inline_nfun('kapok.list', 'to-float', 1) -> ?nfun(erlang, list_to_float, 1);
?inline_nfun('kapok.list', 'to-integer', 1) -> ?nfun(erlang, list_to_integer, 1);
?inline_nfun('kapok.list', 'to-integer', 2) -> ?nfun(erlang, list_to_integer, 2);
?inline_nfun('kapok.list', 'to-tuple', 2) -> ?nfun(erlang, list_to_tuple, 1);

%% tuple
?inline_nfun('kapok.tuple', 'append', 2) -> ?nfun(erlang, append_element, 2);
?inline_nfun('kapok.tuple', 'to-list', 1) -> ?nfun(erlang, tuple_to_list, 1);

inline(_M, _F, _A, _P) ->
  %% TODO add impl
  false.


%% Rewrite rules
%% Rewrite rules are more complex than regular inlining code.
%% It receives all remote call arguments and return quoted
%% expressions with the new enviroment.


rewrite(Meta, Module, Fun, FunMeta, _Arity, _ParaType, Args) ->
  case rewrite(Module, Fun, Args) of
    {M, F, Args1} ->
      Dot = {dot, FunMeta, {{identifier, FunMeta, M}, {identifier, FunMeta, F}}},
      {list, Meta, [Dot | Args1]};
    false ->
      false
  end.

%% Simple rewrite rules

rewrite('kapok.core', 'elem', [Tuple, Index]) ->
  {erlang, element, [increment(Index), Tuple]};
rewrite('kapok.core', 'set-elem', [Tuple, Index, Value]) ->
  {erlang, setelement, [increment(Index), Tuple, Value]};
rewrite('kapok.tuple', 'duplicate', [Data, Size]) ->
  {erlang, make_tuple, [Size, Data]};
rewrite('kapok.tuple', 'insert-at', [Tuple, Index, Term]) ->
  {erlang, insert_element, [increment(Index), Tuple, Term]};
rewrite('kapok.tuple', 'delete-at', [Tuple, Index]) ->
  {erlang, delete_element, [increment(Index), Tuple]};
rewrite(_Receiver, _Fun, _Args) ->
  false.

%% Helpers

increment(Expr) ->
  Dot = {dot, [], {{dot, [], {{identifier, [], 'kapok'},
                              {identifier, [], 'core'}}},
                   {identifier, [], '+'}}},
  {list, [], [Dot, {number, [], 1}, Expr]}.

%% rewrite core.set-elem/3, index + 1
