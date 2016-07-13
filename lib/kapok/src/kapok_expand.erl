%%
-module(kapok_expand).
-export([macroexpand/2,
         macroexpand_1/2]).
-import(kapok_scanner, [token_meta/1, token_text/1]).
-include("kapok.hrl").

macroexpand(List, Env) when is_list(List) ->
  lists:mapfoldl(fun macroexpand/2, Env, List);
macroexpand(Ast, Env) ->
  {EAst, EEnv, Expanded} = macroexpand_1(Ast, Env),
  case Expanded of
    true -> macroexpand(EAst, EEnv);
    false -> {EAst, EEnv}
  end.

%% a list of ast
macroexpand_1(List, Env) when is_list(List) ->
  macroexpand_list(List, Env);

%% list
macroexpand_1({list, _, [{identifier, _, Id} | _T]} = Ast, Env) when ?is_def(Id) ->
  %% TODO move defs into kapok.core as predefined macros
  {Ast, Env, false};
macroexpand_1({list, Meta, [{identifier, _, Id} | T]} = Ast, Env) ->
  Arity = length(T),
  {R, Env1} = kapok_dispatch:find_local_macro(Meta, {Id, Arity}, Env),
  case R of
    {M, F, A, P} ->
      NewArgs = kapok_translate:construct_new_args('expand', Arity, A, P, T),
      {EAst, EEnv} = kapok_dispatch:expand_macro_named(Meta, M, F, A, NewArgs, Env1),
      {EAst, EEnv, true};
    false ->
      macroexpand_list(Ast, Env)
  end;
macroexpand_1({list, Meta, [{dot, _, {M, F}} | T]} = Ast, Env) ->
  Arity = length(T),
  {R, Env1} = kapok_dispatch:find_remote_macro(Meta, M, {F, Arity}, Env),
  case R of
    {M, F, A, P} ->
      NewArgs = kapok_translate:construct_new_args('expand', Arity, A, P, T),
      {EAst, EEnv} = kapok_dispatch:expand_macro_named(Meta, M, F, A, NewArgs, Env1),
      {EAst, EEnv, true};
    false ->
      macroexpand_list(Ast, Env1)
  end;
%% list and literal list
macroexpand_1({Category, _, _} = Ast, Env) when ?is_list(Category) ->
  macroexpand_list(Ast, Env);
%% cons_list
macroexpand_1({cons_list, Meta, {Head, Tail}}, Env) ->
  {EHead, EEnv1, Expanded1} = macroexpand_1(Head, Env),
  {ETail, EEnv2, Expanded2} = macroexpand_1(Tail, EEnv1),
  {{cons_list, Meta, {EHead, ETail}}, EEnv2, Expanded1 orelse Expanded2};
%% non-list containers
macroexpand_1({Category, Meta, Args}, Env)
    when Category == 'bitstring', is_list(Args);
         Category == 'tuple';
         Category == 'map';
         Category == 'set' ->
  {EArgs, EEnv, Expanded} = macroexpand_list(Args, Env),
  {{Category, Meta, EArgs}, EEnv, Expanded};

%% macro special forms
%% quote a list and literal list
macroexpand_1({quote, _, {Category, Meta, List}}, Env) when ?is_list(Category) ->
  {EList, EEnv, Expanded} = macroexpand_quote_list(List, Env),
  {{Category, Meta, EList}, EEnv, Expanded};
%% quote an atom
macroexpand_1({quote, _, Arg}, Env) ->
  {Arg, Env, false};

%% backquote a quote expression
macroexpand_1({backquote, _, {quote, Meta, Arg}}, Env) ->
  {EArg, EEnv, Expanded} = macroexpand_1({backquote, Meta, Arg}, Env),
  {{quote, Meta, EArg}, EEnv, Expanded};
%% backquote a backquote expression
macroexpand_1({backquote, _, {backquote, Meta, Arg}}, #{macro_context := Context} = Env) ->
  B = ?m(Context, backquote_level),
  Context1 = Context#{backquote_level => B + 1},
  {EArg, EEnv, Expanded} = macroexpand_1({backquote, Meta, Arg}, Env#{macro_context => Context1}),
  {{backquote, Meta, EArg}, EEnv#{macro_context => Context}, Expanded};
%% backquote an unquote expression
macroexpand_1({backquote, _, {unquote, Meta, Arg}}, #{macro_context := Context} = Env) ->
  B = ?m(Context, backquote_level),
  U = ?m(Context, unquote_level),
  if
    U == B ->
      {EArg, EEnv} = kapok_translate:eval(Arg, Env),
      {EArg, EEnv, true};
    U < B ->
      Context1 = Context#{unquote_level => U + 1},
      {EArg, EEnv, Expanded} = macroexpand_1({backquote, Meta, Arg}, Env#{macro_context => Context1}),
      {{unquote, Meta, EArg}, EEnv#{macro_context => Context}, Expanded};
    U > B ->
      kapok_error:compile_error(Meta, ?m(Env, file), "unquote doesn't match backquote")
  end;
%% backquote an unquote_splicing expression
macroexpand_1({backquote, _, {unquote_splicing, Meta, Arg}}, #{macro_context := Context} = Env) ->
  B = ?m(Context, backquote_level),
  U = ?m(Context, unquote_level),
  if
    U == B ->
      {EArg, EEnv} = kapok_translate:eval(Arg, Env),
      {{evaluated_unquote_splicing, Meta, EArg}, EEnv};
    U < B ->
      Context1 = Context#{unquote_level => U + 1},
      {EArg, EEnv1, Expanded} = macroexpand_1({backquote, Meta, Arg}, Env#{macro_context => Context1}),
      {{unquote_splicing, Meta, EArg}, EEnv1#{macro_context => Context}, Expanded};
    U > B ->
      kapok_error:compile_error(Meta, ?m(Env, file), "unquote_splicing doesn't match backquote")
  end;
%% backquote a list and literal_list
macroexpand_1({backquote, _, {Category, _, _} = Ast}, Env) when ?is_list(Category) ->
  io:format("backquote a list: ~p~n", [Ast]),
  macroexpand_backquote_list(Ast, Env);
%% backquote a cons_list
macroexpand_1({backquote, _, {cons_list, Meta, {Head, Tail}}}, Env) ->
  {EHead, EEnv1, Expanded1} = macroexpand_list(Head, Env),
  {ETail, EEnv2, Expanded2} = macroexpand_1({backquote, token_meta(Tail), Tail}, EEnv1),
  {{cons_list, Meta, {EHead, ETail}}, EEnv2, Expanded1 orelse Expanded2};
%% backquote non-list containers
macroexpand_1({backquote, _, {Category, Meta, Args}}, Env)
    when Category == 'bitstring', is_list(Args);
         Category == 'tuple';
         Category == 'map';
         Category == 'set' ->
  L = lists:map(fun(X) -> {backquote, token_meta(X), X} end, Args),
  {EL, EEnv, Expanded} = macroexpand_1(L, Env),
  {{Category, Meta, EL}, EEnv, Expanded};
%% backquote an atom (including identifier and dot_identifier)
macroexpand_1({backquote, Meta, Arg}, Env) ->
  macroexpand_1({quote, Meta, Arg}, Env);
%% atom
macroexpand_1(Ast, Env) ->
  {Ast, Env, false}.

macroexpand_list({Category, Meta, List}, Env) when ?is_list(Category), is_list(List) ->
  {EList, EEnv, Expanded} = macroexpand_list(List, Env),
  {{Category, Meta, EList}, EEnv, Expanded};
macroexpand_list(List, Env) when is_list(List) ->
  macroexpand_list(List, Env, fun(Ast) -> Ast end).

macroexpand_list(List, Env, Transform) when is_list(List) ->
  {L, {EEnv, Expanded}} = lists:mapfoldl(fun(Ast, {Env1, Expanded1}) ->
                                             Ast1 = Transform(Ast),
                                             {EAst2, EEnv2, Expanded2} = macroexpand_1(Ast1, Env1),
                                             {EAst2, {EEnv2, Expanded2 orelse Expanded1}}
                                         end,
                                         {Env, false},
                                         List),
  {L, EEnv, Expanded}.

macroexpand_quote_list(List, Env) ->
  macroexpand_list(List, Env, fun(Ast) -> {quote, token_meta(Ast), Ast} end).

macroexpand_backquote_list({Category, Meta, List}, Env) ->
  {EL, {Env2, Expanded}} = lists:mapfoldl(fun(Ast, {Env1, Expanded1}) ->
                                              {EAst, Env2, Expanded2} = macroexpand_1({backquote, token_meta(Ast), Ast}, Env1),
                                              {EAst, {Env2, Expanded2 orelse Expanded1}}
                                          end,
                                          {Env, false},
                                          List),
  Ast = macroexpand_backquote_list(Category, Meta, lists:reverse(EL), [], []),
  {Ast, Env2, Expanded}.

macroexpand_backquote_list(Category, Meta, [], Atoms, Acc) ->
  {list, Meta, [{dot, Meta, ['kapok_macro', 'list*']}, {Category, Meta, Atoms}, {Category, Meta, Acc}]};
macroexpand_backquote_list(Category, Meta, [{evaluated_unquote_splicing, Meta1, Arg}|T], [], Acc) ->
  Acc1 = [{list, Meta1, [{dot, Meta1, ['kapok_macro', 'append']}, Arg | {Category, Meta1, Acc}]}],
  macroexpand_backquote_list(Category, Meta, T, [], Acc1);
macroexpand_backquote_list(Category, Meta, [{evaluated_unquote_splicing, Meta1, Arg}|T], Atoms, Acc) ->
  Acc1 = {list, Meta1, [{dot, Meta1, ['kapok_macro', 'list*']}, {Category, Meta1, Atoms}, {Category, Meta1, Acc}]},
  Acc2 = [{list, Meta1, [{dot, Meta1, ['kapok_macro', 'append']}, Arg | {Category, Meta1, Acc1}]}],
  macroexpand_backquote_list(Category, Meta, T, [], Acc2);
macroexpand_backquote_list(Category, Meta, [Ast|T], Atoms, Acc) ->
  macroexpand_backquote_list(Category, Meta, T, [Ast | Atoms], Acc).
