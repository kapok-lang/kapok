-module(kapok_macro).
-export(['__info__'/1,
         append/2,
         'list*'/2]).
-import(kapok_scanner, [token_meta/1, token_text/1]).
-include("kapok.hrl").

'__info__'(functions) ->
  [];
'__info__'(macros) ->
  [{append, 2, 'normal'},
   {'list*', 2, 'normal'}].

append(Ast1, Ast2) ->
  case kapok_compiler:get_opt(debug) of
    true -> io:format("--- call kapok_expand:append() ---~nAst1: ~p~nAst2: ~p~n===~n", [Ast1, Ast2]);
    false -> ok
  end,
  EAst1 = expand(Ast1),
  EAst2 = expand(Ast2),
  do_append(EAst1, EAst2).

do_append({Category1, Meta1, List1}, {Category2, _, List2})
    when ?is_list(Category1), ?is_list(Category2), is_list(List1), is_list(List2) ->
  {Category2, Meta1, lists:append(List1, List2)};
do_append(List1, {Category2, Meta2, List2})
    when ?is_list(Category2), is_list(List1), is_list(List2) ->
  {Category2, Meta2, lists:append(List1, List2)};
do_append(Ast1, Ast2) ->
  kapok_error:compile_error(token_meta(Ast1), <<"in macro:append()">>, "invalid arguments, (~s, ~s)", [token_text(Ast1), token_text(Ast2)]).

'list*'(Ast1, Ast2) ->
  case kapok_compiler:get_opt(debug) of
    true -> io:format("--- call kapok_expand:list* List ---~nAst1: ~p~nAst2: ~p~n===~n", [Ast1, Ast2]);
    false -> ok
  end,
  EAst1 = expand(Ast1),
  EAst2 = expand(Ast2),
  'do_list*'(EAst1, EAst2).

'do_list*'({Category1, Meta1, List1}, {Category2, _Meta2, List2})
    when ?is_list(Category1), is_list(List1), ?is_list(Category2), is_list(List2) ->
  {Category1, Meta1, lists:append(List1, List2)};
'do_list*'(Ast1, Ast2) ->
  kapok_error:compile_error(token_meta(Ast1), <<"in macro:list*()">>, "invalid arguments: (~s, ~s)", [token_text(Ast1), token_text(Ast2)]).

expand({list, _, [{dot, _, {Module, Fun}} | T]})
    when Module == 'kapok_macro' andalso (Fun == 'append' orelse Fun == 'list*') ->
  erlang:apply(Module, Fun, T);
expand({list, Meta, List}) ->
  EList = lists:map(fun expand/1, List),
  {list, Meta, EList};
expand(Ast) ->
  Ast.



