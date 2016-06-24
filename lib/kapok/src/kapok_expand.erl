%%
-module(kapok_expand).
-export([expand_all/2,
         expand/2,
         quote/1]).
-include("kapok.hrl").

expand_all(Ast, Env) ->
  {EAst, EEnv, _} = do_expand_all(Ast, Env, false),
  {EAst, EEnv}.

do_expand_all(Ast, Env, PreviousExpanded) ->
  {EAst, EEnv, Expanded} = expand(Ast, Env),
  case Expanded of
    true -> do_expand_all(EAst, EEnv, true);
    false -> {EAst, EEnv, PreviousExpanded}
  end.

macroexpand(Ast, Env) ->
  todo.

macroexpand_1(Ast, Env) ->
  todo.

%% macro special forms

expand({quote, Meta, Arg} = Ast, #{macro_context := Context} = Env) ->
  case is_inside_backquote(Context) of
    true ->
      {EArg, NewEnv, Expanded} = expand(Arg, Env),
      {{quote, Meta, EArg}, NewEnv, Expanded};
    false ->
      {quote(Ast), Env, false}
  end;

expand({backquote, Meta, Arg}, #{macro_context := Context} = Env) ->
  #{backquote_level := B} = Context,
  NewContext = Context#{backquote_level => B + 1},
  {EArg, EEnv, Expanded} = expand(Arg, Env#{macro_context => NewContext}),
  NewEnv = EEnv#{macro_context => Context},
  case is_inside_backquote(Context) of
    true ->
      {{backquote, Meta, EArg}, NewEnv, Expanded};
    false ->
      {quote(EArg), NewEnv, Expanded}
  end;

expand({unquote, Meta, Arg}, #{macro_context := Context} = Env) ->
  #{backquote_level := B, unquote_level := U} = Context,
  CurrentU = U + 1,
  if
    CurrentU == B ->
      {EArg, NewEnv} = eval_in_expand(Arg, Env),
      {EArg, NewEnv#{macro_context => Context}, true};
    CurrentU < B ->
      NewContext = Context#{unquote_level => CurrentU},
      {EArg, NewEnv, Expanded} = expand(Arg, Env#{macro_context => NewContext}),
      {{unquote, Meta, EArg}, NewEnv#{macro_context => Context}, Expanded};
    CurrentU > B ->
      kapok_error:compile_error(Meta, ?m(Env, file), "unquote outside backquote")
  end;

expand({unquote_splicing, Meta, Arg}, #{macro_context := Context} = Env) ->
  #{backquote_level := B, unquote_level := U} = Context,
  CurrentU = U + 1,
  if
    CurrentU == B ->
      {EValue, NewEnv} = eval_in_expand(Arg, Env),
      case EValue of
        {Category, _, List} when ?is_list(Category) ->
          {{unquote_splicing_value, Meta, List}, NewEnv#{macro_context => Context}, true};
        _ ->
          kapok_error:compile_error(Meta, ?m(Env, file), "invalid argument for unquote splice: ~p, it should be a list", [EValue])
      end;
    CurrentU < B ->
      NewContext = Context#{unquote_level => CurrentU},
      {EArg, NewEnv, Expanded} = expand(Arg, Env#{macro_context => NewContext}),
      {{unquote_splicing, Meta, EArg}, NewEnv#{macro_context => Context}, Expanded};
    CurrentU > B ->
      kapok_error:compile_error(Meta, ?m(Env, file), "unquote_splicing outside backquote")
  end;


%% Containers

%% bitstring
expand({bitstring, Meta, Args}, Env) ->
  {EArgs, NewEnv, Expanded} = expand(Args, Env),
  {{bitstring, Meta, EArgs}, NewEnv, Expanded};

%% list

expand({literal_list, Meta, Args}, Env) ->
  {EArgs, NewEnv, Expanded} = expand(Args, Env),
  {{literal_list, Meta, EArgs}, NewEnv, Expanded};

expand({cons_list, Meta, {Head, Tail}}, Env) ->
  {EHead, TEnv, ExpandedHead} = expand(Head, Env),
  {ETail, TEnv1, ExpandedTail} = expand(Tail, TEnv),
  {{cons_list, Meta, {EHead, ETail}}, TEnv1, ExpandedHead orelse ExpandedTail};

expand({list, Meta, [{Category, _, Id} | Args]} = Ast, #{macro_context := Context} = Env)
    when ?is_local_id(Category) ->
  case is_inside_backquote(Context) of
    true -> expand_ast_list(Ast, Env);
    false ->
      Arity = length(Args),
      {R, Env1} = kapok_dispatch:find_local_macro(Meta, {Id, Arity}, Env),
      case R of
        {M, F, A, P} ->
          NewArgs = kapok_translate:construct_new_args('expand', Arity, A, P, Args),
          kapok_dispatch:expand_macro_named(Meta, M, F, A, NewArgs, Env1);
        false ->
          expand_ast_list(Ast, Env1)
      end
  end;
expand({list, Meta, [{dot, _, {M, F}} | Args]} = Ast, #{macro_context := Context} = Env) ->
  case is_inside_backquote(Context) of
    true -> expand_ast_list(Ast, Env);
    false ->
      Arity = length(Args),
      {R, Env1} = kapok_dispatch:find_remote_macro(Meta, M, {F, Arity}, Env),
      case R of
        {M, F, A, P} ->
          NewArgs = kapok_translate:construct_new_args('expand', Arity, A, P, Args),
          kapok_dispatch:expand_macro_named(Meta, M, F, A, NewArgs, Env1);
        false ->
          expand_ast_list(Ast, Env1)
      end
  end;
expand({list, _, _} = Ast, Env) ->
  expand_ast_list(Ast, Env);

%% tuple
expand({tuple, Meta, Args}, Env) ->
  {EArgs, NewEnv, Expanded} = expand(Args, Env),
  {{tuple, Meta, EArgs}, NewEnv, Expanded};

%% map
expand({map, Meta, Args}, Env) ->
  {EArgs, NewEnv, Expanded} = expand(Args, Env),
  {{map, Meta, EArgs}, NewEnv, Expanded};

%% set

expand({set, Meta, Args}, Env) ->
  {EArgs, NewEnv, Expanded} = expand(Args, Env),
  {{set, Meta, EArgs}, NewEnv, Expanded};

expand(List, Env) when is_list(List) ->
  expand_list(List, Env);

expand(Ast, Env) ->
  %% the default handler, which handles
  %% number, keyword, atom, identifier, strings(binary string and list string)
  {Ast, Env, false}.

%% Helpers

expand_list(List, Env) ->
  expand_list(List, fun expand/2, Env).
expand_list(List, Fun, Env) ->
  expand_list(List, Fun, Env, false, []).
expand_list([H|T], Fun, Env, Expanded, Acc) ->
  {EArg, NewEnv, IsExpanded} = Fun(H, Env),
  NewAcc = case EArg of
             {unquote_splicing_value, _, List} -> lists:reverse(List) ++ Acc;
             _ -> [EArg | Acc]
           end,
  expand_list(T, Fun, NewEnv, Expanded or IsExpanded, NewAcc);
expand_list([], _Fun, Env, Expanded, Acc) ->
  {lists:reverse(Acc), Env, Expanded}.

expand_ast_list({list, Meta, Args}, Env) ->
  {EArgs, NewEnv, Expanded} = expand_list(Args, Env),
  {{list, Meta, EArgs}, NewEnv, Expanded}.


%% Quotes an expression and return its AST.
%% tuple
quote(Arg) when is_tuple(Arg) ->
  {tuple, [], lists:map(fun quote/1, tuple_to_list(Arg))};
%% list
quote(Args) when is_list(Args) ->
  {literal_list, [], lists:map(fun quote/1, Args)};
%% map
quote(Arg) when is_map(Arg) ->
  {map, [], lists:reverse(lists:foldl(fun({K, V}, Acc) -> [quote(V), quote(K) | Acc] end,
                                      [],
                                      maps:to_list(Arg)))};
%% atom
quote(Arg) when is_atom(Arg) ->
  {atom, [], Arg};
%% number
quote(Number) when is_number(Number) ->
  {number, [], Number};
%% list string and binary string
quote(Arg) when is_binary(Arg) ->
  {bitstring, [], {binary_string, [], Arg}}.

eval_in_expand(Arg, Env) ->
  case is_list_ast(Arg) of
    true ->
      {ErlValue, NewEnv} = kapok_compiler:ast(Arg, kapok_env:reset_macro_context(Env)),
      {quote(ErlValue), NewEnv};
    false ->
      {Arg, Env}
  end.


is_inside_backquote(#{backquote_level := B} = _Context) ->
  B > 0.

is_list_ast({list, _, _}) ->
  true;
is_list_ast(_) ->
  false.

%% (quote (a b c)) -> (list (quote a) (quote b) (quote c))
transform_quote_list(Meta, {list, _, List}, QuoteType) ->
  {list, Meta, lists:map(fun({_, MetaElem, _} = Ast) -> {QuoteType, MetaElem, Ast} end, List)}.

