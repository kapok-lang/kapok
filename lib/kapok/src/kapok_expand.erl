%%
-module(kapok_expand).
-export([macroexpand/2,
         macroexpand_1/2,
         expand/2,
         quote/1]).
-import(kapok_scanner, [token_meta/1, token_text/1]).
-include("kapok.hrl").

macroexpand(Ast, Env) ->
  {EAst, EEnv, Expanded} = macroexpand_1(Ast, Env),
  case Expanded of
    true -> macroexpand(EAst, EEnv);
    fale -> {EAst, EEnv}
  end.

macroexpand_1({quote, Meta, {list, _, List}}, Env) ->
  {EList, EEnv} = list:mapfoldl(fun(X, E) -> macroexpand_1({quote, token_meta(X), X}, E) end,
                                Env,
                                List),
  {{list, Meta, EList}, EEnv, false};
macroexpand_1({quote, _, {C, _, _}} = Ast, Env) when ?is_dot_id(C) ->
  {Ast, Env, false};
macroexpand_1({quote, _, Arg}, Env) ->
  {Arg, Env, false};
macroexpand_1({backquote, Meta, {list, _, List}}, #{macro_context := Context} = Env) ->
  #{backquote_level := B} = Context,
  NewContext = Context#{backquote_level => B + 1},
  Env1 = Env#{macro_context => NewContext},
  macroexpand_backquote_list(Meta, List, Env1);
macroexpand_1({backquote, _, {unquote, _, Arg}}, Env) ->
  {Arg, Env, true};
macroexpand_1({backquote, Meta, {unquote_splicing, _, _} = Token}, Env) ->
  %% not a well-formed backquote expression, output text of Arg
  kapok_error:compile_error(Meta, ?m(Env, file), "unquote_splicing ~s outside a list", [token_text(Token)]);
macroexpand_1({backquote, Meta, Arg}, Env) ->
  macroexpand_1({quote, Meta, Arg}, Env);
macroexpand_1(Ast, Env) ->
  {Ast, Env, false}.

macroexpand_backquote_list(Meta, List, Env) ->
  macroexpand_backquote_list(Meta, List, [], false, Env).
macroexpand_backquote_list(Meta, [], Acc, Expanded, Env) ->
  {Splicing, L} = lists:foldl(fun({unquote_splicing_value, _, V}, {false, []}) ->
                                  {true, [V]};
                                 ({unquote_splicing_value, Meta1, V}, {false, Acc1}) ->
                                  {true, {list, Meta1, [{identifier, Meta1, 'append'}, V , Acc1]}};
                                 (X, {B, Acc1}) ->
                                  {B, [X | Acc1]}
                              end,
                              {false, []},
                              Acc),
  Op = case Splicing of
           false -> 'list';
           true -> 'list*'
       end,
  EAst = {list, Meta, [{identifier, Meta, Op} | L]},
  {EAst, Env, Expanded};
macroexpand_backquote_list(Meta, [{unquote, Meta1, Arg} | T], Acc, Expanded, #{macro_context := Context} = Env) ->
  #{backquote_level := B, unquote_level := U} = Context,
  CurrentU = U + 1,
  if
    CurrentU == B ->
      macroexpand_backquote_list(Meta, T, [Arg | Acc], true, Env);
    CurrentU < B ->
      NewContext = Context#{unquote_level => CurrentU},
      Env1 = Env#{macro_context => NewContext},
      {EArg, EEnv, Expanded1} = macroexpand_1(Arg, Env1),
      Acc1 = [{list, Meta1, [{identifier, Meta1, 'unquote'}, EArg]} | Acc],
      macroexpand_backquote_list(Meta, T, Acc1, Expanded1 orelse Expanded, EEnv#{macro_context => Context});
    CurrentU > B ->
      kapok_error:compile_error(Meta1, ?m(Env, file), "unquote outside backquote")
  end;
macroexpand_backquote_list(Meta, [{unquote_splicing, Meta1, Arg} | T], Acc, Expanded, #{macro_context := Context} = Env) ->
  #{backquote_level := B, unquote_level := U} = Context,
  CurrentU = U + 1,
  if
    CurrentU == B ->
      R = {unquote_splicing_value, Meta1, Arg},
      macroexpand_backquote_list(Meta, T, [R | Acc], true, Env);
    CurrentU < B ->
      NewContext = Context#{unquote_level => CurrentU},
      Env1 = Env#{macro_context => NewContext},
      {EArg, EEnv, Expanded1} = macroexpand_1(Arg, Env1),
      Acc1 = [{list, Meta1, [{identifier, Meta1, 'unquote-splicing'}, EArg]} | Acc],
      macroexpand_backquote_list(Meta, T, Acc1, Expanded1 orelse Expanded, EEnv#{macro_context => Context});
    CurrentU > B ->
      kapok_error:compile_error(Meta1, ?m(Env, file), "unquote_splicing outside backquote")
  end;
macroexpand_backquote_list(Meta, [H | T], Acc, Expanded, Env) ->
  macroexpand_backquote_list(Meta, T, [H | Acc], Expanded, Env).

%% macro special forms

expand({quote, Meta, Arg} = Ast, #{macro_context := Context} = Env) ->
  case is_inside_backquote(Context) of
    true ->
      {EArg, NewEnv} = expand(Arg, Env),
      {{quote, Meta, EArg}, NewEnv};
    false ->
      %% TODO add unquote/unquote_splicing backquote pair checking
      {Ast, Env}
  end;

expand({backquote, Meta, Arg}, #{macro_context := Context} = Env) ->
  #{backquote_level := B} = Context,
  NewContext = Context#{backquote_level => B + 1},
  {EArg, EEnv} = expand(Arg, Env#{macro_context => NewContext}),
  NewEnv = EEnv#{macro_context => Context},
  case is_inside_backquote(Context) of
    true ->
      {{backquote, Meta, EArg}, NewEnv};
    false ->
      {{backquote_expended, Meta, EArg}, NewEnv}
  end;

expand({unquote, Meta, Arg}, #{macro_context := Context} = Env) ->
  #{backquote_level := B, unquote_level := U} = Context,
  CurrentU = U + 1,
  if
    CurrentU == B ->
      {EArg, NewEnv} = eval_in_expand(Arg, Env),
      io:format("after eval_in_expand: ~p~n", [EArg]),
      {EArg, NewEnv#{macro_context => Context}};
    CurrentU < B ->
      NewContext = Context#{unquote_level => CurrentU},
      {EArg, NewEnv} = expand(Arg, Env#{macro_context => NewContext}),
      {{unquote, Meta, EArg}, NewEnv#{macro_context => Context}};
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
          {{unquote_splicing_value, Meta, List}, NewEnv#{macro_context => Context}};
        _ ->
          kapok_error:compile_error(Meta, ?m(Env, file), "invalid argument for unquote splice: ~p, it should be a list", [EValue])
      end;
    CurrentU < B ->
      NewContext = Context#{unquote_level => CurrentU},
      {EArg, NewEnv} = expand(Arg, Env#{macro_context => NewContext}),
      {{unquote_splicing, Meta, EArg}, NewEnv#{macro_context => Context}};
    CurrentU > B ->
      kapok_error:compile_error(Meta, ?m(Env, file), "unquote_splicing outside backquote")
  end;


%% Containers

%% bitstring
expand({bitstring, Meta, Args}, Env) ->
  {EArgs, NewEnv} = expand(Args, Env),
  {{bitstring, Meta, EArgs}, NewEnv};

%% list

expand({literal_list, Meta, Args}, Env) ->
  {EArgs, NewEnv} = expand(Args, Env),
  {{literal_list, Meta, EArgs}, NewEnv};

expand({cons_list, Meta, {Head, Tail}}, Env) ->
  {EHead, TEnv} = expand(Head, Env),
  {ETail, TEnv1} = expand(Tail, TEnv),
  {{cons_list, Meta, {EHead, ETail}}, TEnv1};

expand({list, _, [{identifier, _, Id} | _Args]} = Ast, #{macro_context := Context} = Env) when ?is_def(Id) ->
  case is_inside_backquote(Context) of
    true -> expand_ast_list(Ast, Env);
    false -> {Ast, Env}
  end;
expand({list, Meta, [{identifier, _, Id} | Args]} = Ast, #{macro_context := Context} = Env) ->
  case is_inside_backquote(Context) of
    true ->
      expand_ast_list(Ast, Env);
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
    true ->
      expand_ast_list(Ast, Env);
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
  {EArgs, NewEnv} = expand(Args, Env),
  {{tuple, Meta, EArgs}, NewEnv};

%% map
expand({map, Meta, Args}, Env) ->
  {EArgs, NewEnv} = expand(Args, Env),
  {{map, Meta, EArgs}, NewEnv};

%% set

expand({set, Meta, Args}, Env) ->
  {EArgs, NewEnv} = expand(Args, Env),
  {{set, Meta, EArgs}, NewEnv};

expand(List, Env) when is_list(List) ->
  expand_list(List, Env);

expand(Ast, Env) ->
  %% the default handler, which handles
  %% number, keyword, atom, identifier, strings(binary string and list string)
  {Ast, Env}.

%% Helpers

expand_list(List, Env) ->
  expand_list(List, fun expand/2, Env).
expand_list(List, Fun, Env) ->
  expand_list(List, Fun, Env, []).
expand_list([H|T], Fun, Env, Acc) ->
  {EArg, NewEnv} = Fun(H, Env),
  NewAcc = case EArg of
             {unquote_splicing_value, _, List} -> lists:reverse(List) ++ Acc;
             _ -> [EArg | Acc]
           end,
  expand_list(T, Fun, NewEnv, NewAcc);
expand_list([], _Fun, Env, Acc) ->
  {lists:reverse(Acc), Env}.

expand_ast_list({list, Meta, Args}, Env) ->
  {EArgs, EEnv} = expand_list(Args, Env),
  {{list, Meta, EArgs}, EEnv}.

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

eval_in_expand({list, _, _} = Ast, Env) ->
  {ErlValue, NewEnv} = kapok_compiler:ast(Ast, kapok_env:reset_macro_context(Env)),
  {quote(ErlValue), NewEnv};
eval_in_expand(Ast, Env) ->
  {Ast, Env}.

is_inside_backquote(#{backquote_level := B} = _Context) ->
  B > 0.

