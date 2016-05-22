%%
-module(kapok_expand).
-export([expand_all/2,
         expand_1/2,
         expand/2]).
-include("kapok.hrl").

expand_all(Ast, Env) ->
%%  io:format("^^^ to expand ~p~n", [Ast]),
  {EAst, NewEnv, Expanded} = expand(Ast, Env),
%%  io:format("*** expand ~p to: ~p~n", [Expanded, EAst]),
  case Expanded of
    true -> expand_all(EAst, NewEnv);
    _ -> {EAst, NewEnv}
  end.

expand_1(Ast, Env) ->
  expand(Ast, Env).

%% macro special forms

expand({quote, Meta, Arg} = Ast, #{macro_context := Context} = Env) ->
  case is_not_inside_quote(Context) of
    true ->
      case is_list_ast(Arg) of
        true -> {transform_quote_list(Meta, Arg, 'quote'), Env, true};
        false -> {Ast, Env, false}
      end;
    false ->
      NewContext = Context#{quote => true},
      {EArg, NewEnv, Expanded} = expand(Arg, Env#{macro_context => NewContext}),
      {{quote, Meta, EArg}, NewEnv#{macro_context => Context}, Expanded}
  end;

expand({backquote, Meta, Arg}, #{macro_context := Context} = Env) ->
  io:format("expand backquote arg: ~p~n", [Arg]),
  case is_not_inside_quote(Context) of
    true ->
      case is_list_ast(Arg) of
        true ->
          {transform_quote_list(Meta, Arg, 'backquote'), Env, true};
        false ->
          #{backquote_level := B} = Context,
          NewContext = Context#{backquote_level => B + 1, quote => true},
          {EAst, NewEnv, Expanded} = expand(Arg, Env#{macro_context => NewContext}),
          case Expanded of
            true -> {EAst, NewEnv#{macro_context => Context}, true};
            false -> {{quote, Meta, EAst}, NewEnv#{macro_context => Context}, false}
          end
      end;
    false ->
      #{backquote_level := B} = Context,
      NewContext = Context#{backquote_level => B + 1, quote => true},
      {EArg, NewEnv, _Expanded} = expand(Arg, Env#{macro_context => NewContext}),
      Ast = {backquote, Meta, EArg},
      io:format("expand backquote return: ~p~n", [Ast]),
      {Ast, NewEnv#{macro_context => Context}, true}
  end;

expand({unquote, Meta, Arg}, #{macro_context := Context} = Env) ->
  #{backquote_level := B, unquote_level := U} = Context,
  CurrentU = U + 1,
  io:format("----------- ~p~n", [CurrentU]),
  if
    CurrentU == B ->
      {EArg, NewEnv} = case is_list_ast(Arg) of
                         true -> kapok_compiler:ast(Arg, kapok_env:reset_macro_context(Env));
                         false -> {Arg, Env}
                       end,
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
      {EAst, NewEnv} = case is_list_ast(Arg) of
                         true -> kapok_compiler:ast(Arg, kapok_env:reset_macro_context(Env));
                         false -> {Arg, Env}
                       end,
      case EAst of
        EList when is_list(EList) ->
          {{unquote_splicing, Meta, EList}, NewEnv#{macro_context => Context}, true};
        _ ->
          #{file := File} = Env,
          kapok_error:compile_error(Meta, File, "unquoie splice should take list")
      end;
    CurrentU < B ->
      NewContext = Context#{unquote_level => CurrentU},
      {EArg, NewEnv, Expanded} = expand(Arg, Env#{macro_context => NewContext}),
      {{unquote_splicing, Meta, EArg}, NewEnv#{macro_context => Context}, Expanded};
    CurrentU > B ->
      kapok_error:compile_error(Meta, ?m(Env, file), "unquote_splicing outside backquote")
  end;


%% identifier
expand({dot, Meta, [Left, Right]}, Env) ->
  {ELeft, LEnv, LExpanded} = expand(Left, Env),
  {ERight, REnv, RExpanded} = expand(Right, LEnv),
  {{dot, Meta, [ELeft, ERight]}, REnv, LExpanded or RExpanded};

%% Containers

%% bitstring
expand({bitstring, Meta, Args}, Env) ->
  {EArgs, NewEnv, Expanded} = expand(Args, Env),
  {{bitstring, Meta, EArgs}, NewEnv, Expanded};

%% list

expand({literal_list, Meta, Args}, Env) ->
  {EArgs, NewEnv, Expanded} = expand_list(Args, Env),
  {{literal_list, Meta, EArgs}, NewEnv, Expanded};

expand({list, Meta, [{identifier, _, Id} | Args]} = Ast, #{macro_context := Context} = Env) ->
  case is_not_inside_quote(Context) of
    true ->
      R = kapok_dispatch:dispatch_local(Meta, Id, Args, Env, fun () -> expand_ast_list(Ast, Env) end),
      {_, B, _} = R,
      io:format("dispatch_local returns, env: ~p~n", [B]),
      R;
    false -> expand_ast_list(Ast, Env)
  end;
expand({list, Meta, [{dot, _, {M, F}} | Args]} = Ast, #{macro_context := Context} = Env) ->
  case is_not_inside_quote(Context) of
    true -> kapok_dispatch:dispatch_remote(Meta, M, F, Args, Env,
                                           fun(_Receiver, _Name, _Args) -> expand_ast_list(Ast, Env) end);
    false -> expand_ast_list(Ast, Env)
  end;
expand({list, _, _} = Ast, Env) ->
  expand_ast_list(Ast, Env);

%% tuple
expand({tuple, Meta, Args}, Env) ->
  {EArgs, NewEnv, Expanded} = expand_list(Args, Env),
  {{tuple, Meta, EArgs}, NewEnv, Expanded};

%% map
expand({map, Meta, Args}, Env) ->
  {EArgs, NewEnv, Expanded} = expand_list(Args, Env),
  {{map, Meta, EArgs}, NewEnv, Expanded};

%% set

expand({set, Meta, Args}, Env) ->
  {EArgs, NewEnv, Expanded} = expand_list(Args, Env),
  {{set, Meta, EArgs}, NewEnv, Expanded};

expand(Ast, Env) ->
  %% the default handler, which handles
  %% number, atom, identifier, strings(binary string and list string)
  {Ast, Env, false}.

%% Helpers

expand_list(List, Env) ->
  expand_list(List, fun expand/2, Env).
expand_list(List, Fun, Env) ->
  expand_list(List, Fun, Env, false, []).
expand_list([H|T], Fun, Env, Expanded, Acc) ->
  {EArg, NewEnv, IsExpanded} = Fun(H, Env),
  NewAcc = case EArg of
             {unquote_splicing, _, EList} -> lists:reverse(EList) ++ Acc;
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
  {list, [], lists:map(fun quote/1, Args)};
%% map
quote(Arg) when is_map(Arg) ->
  {map, [], lists:reverse(lists:foldl(fun ({K, V}, Acc) -> [quote(V), quote(K) | Acc] end,
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

is_not_inside_quote(#{quote := Q, backquote_level := B} = _Context) ->
  (Q == false) orelse (B == 0).

is_list_ast({list, _, _}) ->
  true;
is_list_ast(_) ->
  false.

%% (quote (a b c)) -> (list (quote a) (quote b) (quote c))
transform_quote_list(Meta, {list, _, List}, QuoteType) ->
  {list, Meta, lists:map(fun ({_, MetaElem, _} = Ast) -> {QuoteType, MetaElem, Ast} end, List)}.

