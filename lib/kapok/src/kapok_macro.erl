-module(kapok_macro).
-export([append/2,
         'list*'/2]).
-import(kapok_scanner, [token_meta/1, token_text/1]).
-include("kapok.hrl").

append(Ast1, Ast2) ->
  io:format("--- call kapok_expand:append() ---~nAst1: ~p~nAst2: ~p~n===~n", [Ast1, Ast2]),
  Env = kapok_env:env_for_eval([{line, ?line(token_meta(Ast1))}, {file, <<"in macro:list*()">>}]),
  {EAst1, Env1} = kapok_expand:macroexpand(Ast1, Env),
  {EAst2, _Env2} = kapok_expand:macroexpand(Ast2, Env1),
  do_append(EAst1, EAst2).

do_append({Category1, Meta1, List1}, {Category2, _, List2})
    when ?is_list(Category1), ?is_list(Category2), is_list(List1), is_list(List2) ->
  {Category2, Meta1, lists:append(List1, List2)};
do_append(Ast1, Ast2) ->
  kapok_error:compile_error(token_meta(Ast1), <<"in macro:append()">>, "invalid arguments, (~s, ~s)", [token_text(Ast1), token_text(Ast2)]).

'list*'(Ast1, Ast2) ->
  io:format("--- call kapok_expand:list* List ---~nAst1: ~p~nAst2: ~p~n===~n", [Ast1, Ast2]),
  Env = kapok_env:env_for_eval([{line, ?line(token_meta(Ast1))}, {file, <<"in macro:list*()">>}]),
  {EAst1, Env1} = kapok_expand:macroexpand(Ast1, Env),
  {EAst2, _Env2} = kapok_expand:macroexpand(Ast2, Env1),
  'do_list*'(EAst1, EAst2).

'do_list*'({Category1, Meta1, List1}, {Category2, _Meta2, List2})
    when ?is_list(Category1), is_list(List1), ?is_list(Category2), is_list(List2) ->
  {Category1, Meta1, lists:append(List1, List2)};
'do_list*'(Ast1, Ast2) ->
  kapok_error:compile_error(token_meta(Ast1), <<"in macro:list*()">>, "invalid arguments: (~s, ~s)", [token_text(Ast1), token_text(Ast2)]).


