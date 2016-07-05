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

macroexpand_1(List, Env) when is_list(List) ->
  lists:mapfoldl(fun(X, {Env1, Expanded}) ->
                     {EX, Env2, Expanded1} = macroexpand_1(X, Env1),
                     {EX, {Env2, Expanded1 orelse Expanded}}
                 end,
                 {Env, false},
                 List);
macroexpand_1({list, _, [{identifier, _, Id} | _Args]} = Ast, Env) when ?is_def(Id) ->
  {Ast, Env, false};
macroexpand_1({list, Meta, [{identifier, _, Id} | Args]} = Ast, Env) ->
  Arity = length(Args),
  {R, Env1} = kapok_dispatch:find_local_macro(Meta, {Id, Arity}, Env),
  case R of
    {M, F, A, P} ->
      NewArgs = kapok_translate:construct_new_args('expand', Arity, A, P, Args),
      {EAst, EEnv} = kapok_dispatch:expand_macro_named(Meta, M, F, A, NewArgs, Env1),
      {EAst, EEnv, true};
    false ->
      {Ast, Env1, false}
  end;
macroexpand_1({list, Meta, [{dot, _, {M, F}} | Args]} = Ast, Env) ->
  Arity = length(Args),
  {R, Env1} = kapok_dispatch:find_remote_macro(Meta, M, {F, Arity}, Env),
  case R of
    {M, F, A, P} ->
      NewArgs = kapok_translate:construct_new_args('expand', Arity, A, P, Args),
      {EAst, EEnv} = kapok_dispatch:expand_macro_named(Meta, M, F, A, NewArgs, Env1),
      {EAst, EEnv, true};
    false ->
      {Ast, Env1, false}
  end;
macroexpand_1({list, Meta, [{list, _, _} = H | T]}, Env) ->
  {EH, EEnv, Expanded1} = macroexpand_1(H, Env),
  {ET, EEnv1, Expanded2} = macroexpand_list(T, EEnv),
  case Expanded1 orelse Expanded2 of
    true ->
      {{list, Meta, [EH | ET]}, EEnv1, true};
    false ->
      macroexpand_1({list, Meta, [EH | ET]}, Env)
  end;
macroexpand_1({list, _, _} = Ast, Env) ->
  {Ast, Env, false};

macroexpand_1({quote, Meta, {Category, _, List}}, Env) when ?is_list(Category) ->
  {EList, EEnv, Expanded} = macroexpand_quote_list(List, Env),
  {{Category, Meta, EList}, EEnv, Expanded};
%% TODO quote a cons list
macroexpand_1({quote, _, {C, _, _}} = Ast, Env) when ?is_dot_id(C) ->
  {Ast, Env, false};
%% quote an atom
macroexpand_1({quote, _, Arg}, Env) ->
  {Arg, Env, false};
macroexpand_1({backquote, _, {quote, Meta, Arg}}, Env) ->
  {EArg, EEnv, Expanded} = macroexpand_1({backquote, Meta, Arg}, Env),
  {{quote, Meta, EArg}, EEnv, Expanded};
macroexpand_1({backquote, _, {unquote, Meta, Arg}}, #{macro_context := Context} = Env) ->
  B = ?m(Context, backquote_level),
  U = ?m(Context, unquote_level),
  if
    U == B ->
      kapok_translate:eval(Arg, Env);
    U < B ->
      {EArg, EEnv, Expanded} = macroexpand_1({backquote, Meta, Arg}, Env),
      {{unquote, Meta, EArg}, EEnv, Expanded};
    U > B ->
      kapok_error:compile_error(Meta, ?m(Env, file), "unquote doesn't match backquote")
  end;
macroexpand_1({backquote, _, {unquote_splicing, Meta, Arg}}, #{macro_context := Context} = Env) ->
  B = ?m(Context, backquote_level),
  U = ?m(Context, unquote_level),
  if
    U == B ->
      {EArg, EEnv} = kapok_translate:eval(Arg, Env),
      case EArg of
        {Category, Meta1, List} when ?is_list(Category), is_list(List) ->
          {{evaluated_unquote_splicing, Meta1, List}, EEnv, true};
        _ ->
          kapok_error:compile_error(Meta, ?m(Env, file), "invalid argument for unquote splice: ~s, it should eval to a list ast", [token_text(EArg)])
      end;
    U < B ->
      Context1 = Context#{unquote_level => U + 1},
      Env1 = Env#{macro_context => Context1},
      {EArg, EEnv1, Expanded} = macroexpand_1({backquote, Meta, Arg}, Env1),
      {{unquote_splicing, Meta, EArg}, EEnv1, Expanded};
    U > B ->
      kapok_error:compile_error(Meta, ?m(Env, file), "unquote_splicing doesn't match backquote")
  end;
macroexpand_1({backquote, _, {Category, Meta, Args}}, Env) when ?is_list(Category) ->
  {EList, EEnv, Expanded} = macroexpand_backquote_list(Args, Env),
  {{Category, Meta, EList}, EEnv, Expanded};
%% TODO backquote a cons list
macroexpand_1({backquote, _, {Category, Meta, Args}}, Env)
    when Category == 'bitstring', is_list(Args);
         Category == 'tuple';
         Category == 'map';
         Category == 'set' ->
  L = lists:map(fun(X) -> {backquote, token_meta(X), X} end, Args),
  {EL, EEnv, Expanded} = macroexpand_1(L, Env),
  {{Category, Meta, EL}, EEnv, Expanded};
macroexpand_1({backquote, Meta, Arg}, Env) ->
  macroexpand_1({quote, Meta, Arg}, Env);
macroexpand_1(Ast, Env) ->
  {Ast, Env, false}.

macroexpand_list(List, Env) ->
  {L, {EEnv, Expanded}} = lists:mapfoldl(fun(X, {Env1, Expanded1}) ->
                                             {EX, Env2, Expanded2} = macroexpand_1(X, Env1),
                                             {EX, {Env2, Expanded2 orelse Expanded1}}
                                         end,
                                         {Env, false},
                                         List),
  {L, EEnv, Expanded}.

macroexpand_quote_list(List, Env) ->
  {L, {EEnv, Expanded}} = lists:mapfoldl(fun(X, {Env1, Expanded1}) ->
                                             Ast = {quote, token_meta(X), X},
                                             {EX, Env2, Expanded2} = macroexpand_1(Ast, Env1),
                                             {EX, {Env2, Expanded2 orelse Expanded1}}
                                         end,
                                         {Env, false},
                                         List),
  {L, EEnv, Expanded}.

macroexpand_backquote_list(List, Env) ->
  lists:foldl(fun macroexpand_backquote_list_element/2, {[], Env, false}, List).

macroexpand_backquote_list_element(Ast, {Acc, Env, Expanded}) ->
  {TAst, TEnv, Expanded1} = macroexpand_1({backquote, token_meta(Ast), Ast}, Env),
  Acc1 = case TAst of
           {evaluated_unquote_splicing, _, List} -> lists:reverse(List) ++ Acc;
           _ -> [TAst | Acc]
         end,
  {Acc1, TEnv, Expanded1 orelse Expanded}.

