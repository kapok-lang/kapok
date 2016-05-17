%%
-module(kapok_expand).
-export([expand_all/2,
         expand_1/2,
         expand/2]).
-include("kapok.hrl").

expand_all(Ast, Env) ->
  io:format("^^^ to expand ~p~n", [Ast]),
  {EAst, NewEnv, Expanded} = expand(Ast, Env),
  io:format("*** expand ~p to: ~p~n", [Expanded, EAst]),
  case Expanded of
    true -> expand_all(EAst, NewEnv);
    _ -> {EAst, NewEnv}
  end.

expand_1(Ast, Env) ->
  expand(Ast, Env).

%% block

expand({'__block__', Meta, Args}, Env) ->
  {EArgs, NewEnv, Expanded} = expand_list(Args, Env),
  {{'__block__', Meta, EArgs}, NewEnv, Expanded};

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

expand({list, Meta, Args} = Ast, #{macro_context := Context} = Env) ->
  case is_not_inside_quote(Context) andalso is_macro_call(Ast, Env) of
    true ->
      {Result, Env} = kapok_compiler:ast(Ast, kapok_env:reset_macro_context(Env)),
      {Result, Env, true};
    false ->
      {EArgs, NewEnv, Expanded} = expand_list(Args, Env),
      {{list, Meta, EArgs}, NewEnv, Expanded}
  end;

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

is_not_inside_quote(#{quote := Q, backquote_level := B} = _Context) ->
  (Q == false) orelse (B == 0).

is_list_ast({list, _, _}) ->
  true;
is_list_ast(_) ->
  false.

%% (quote (a b c)) -> (list (quote a) (quote b) (quote c))
transform_quote_list(Meta, {list, _, List}, QuoteType) ->
  {list, Meta, lists:map(fun ({_, MetaElem, _} = Ast) -> {QuoteType, MetaElem, Ast} end, List)}.

is_macro_call({list, Meta, [{identifier, _, Id} | Args]},
                 #{export_macros := ExportMacros,
                   macros := Macros,
                   macro_aliases := MacroAliases} = Env) ->
  FunArity = {Id, length(Args)},
  %% check whether it's a local macro
  case ordsets:is_element(FunArity, ExportMacros) of
    true -> kapok_error:compile_error(Meta, ?m(Env, file), "cannot call macro defined in current module: ~p", [FunArity]);
    false -> ok
  end,
  %% check whether it's a macro call
  case orddict:find(FunArity, Macros) of
    {ok, _} ->
      true;
    error ->
      %% check whether it's a macro alias call.
      case orddict:find(FunArity, MacroAliases) of
        {ok, _} -> true;
        error -> false
      end
  end;
is_macro_call({list, Meta, [{dot, _, {M, F}} | _Args]},
                 #{namespace := Namespace,
                   requires := Requires,
                   module_aliases := ModuleAliases} = Env) ->
  %% check whether the specified module is required or aliased, and macro is imported.
  case M of
    Namespace ->
      kapok_error:compile_error(Meta, ?m(Env, file), "cannot call macro defined in current module: ~p", [F]);
    _ ->
      %% check whether this module is required or aliased
      case ordsets:is_element(M, Requires) of
        true ->
          true;
        false ->
          case orddict:find(M, ModuleAliases) of
            {ok, _} ->
              true;
            error ->
              false
          end
      end
  end;
is_macro_call(_Ast, _Env) ->
  false.

