%%
-module(kapok_expand).
-export(['expand-all'/2,
         'expand-1'/2,
         expand/2]).
-include("kapok.hrl").


'expand-1'(Ast, Env) ->
  expand(Ast, Env).

'expand-all'(Ast, Env) ->
  {EAst, NewEnv, Expanded} = expand(Ast, Env),
  case Expanded of
    true -> 'expand-all'(EAst, NewEnv);
    _ -> {EAst, NewEnv}
  end.

%% block

expand({block, Meta, Args}, Env) ->
  {EArgs, NewEnv, Expanded} = expand(Args, Env),
  {{block, Meta, EArgs}, NewEnv, Expanded};

%% macro special forms

expand({quote, Meta, Arg}, Env) ->
  {EArg, NewEnv, Expanded} = expand(Arg, Env),
  {{quote, Meta, EArg}, NewEnv, Expanded};

expand({backquote, Meta, Arg}, #{macro_context := Context} = Env) ->
  #{backquote_level := B} = Context,
  NewContext = Context#{backquote_level => B + 1},
  {EArg, NewEnv, Expanded} = expand(Arg, Env#{macro_context => NewContext}),
  {{quote, Meta, EArg}, NewEnv#{macro_context => Context}, Expanded};

expand({unquote, Meta, Arg}, #{macro_context := Context} = Env) ->
  #{backquote_level := B, unquote_level := U} = Context,
  CurrentU = U + 1,
  case CurrentU of
    B ->
      kapok_compiler:eval_ast(Arg, Env);
    _ ->
      NewContext = Context#{unquote_level => CurrentU},
      {EArg, NewEnv, Expanded} = expand(Arg, Env#{macro_context => NewContext}),
      {{unquote, Meta, EArg}, NewEnv#{macro_context => Context}, Expanded}
  end;

expand({unquote_splicing, Meta, List}, #{macro_context := Context} = Env) ->
  #{backquote_level := B, unquote_level := U} = Context,
  CurrentU = U + 1,
  case CurrentU of
    B ->
      {EAst, NewEnv} = kapok_compiler:eval_ast(List, Env),
      case EAst of
        EList when is_list(EList) ->
          {{unquote_splicing, Meta, EList}, NewEnv};
        _ ->
          #{file := File} = Env,
          kapok_error:compile_error(Meta, File, "unquoie splice should take list")
      end;
    _ ->
      NewContext = Context#{unquote_level => CurrentU},
      {EArg, NewEnv, Expanded} = expand(List, Env#{macro_context => NewContext}),
      {{unquote_splicing, Meta, EArg}, NewEnv#{macro_context => Context}, Expanded}
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

expand({'list', Meta, Args}, Env) ->
  {EArgs, NewEnv, Expanded} = expand_list(Args, fun expand/2, Env),
  {{'list', Meta, EArgs}, NewEnv, Expanded};

%% tuple
expand({tuple, Meta, Args}, Env) ->
  {EArgs, NewEnv, Expanded} = expand_list(Args, fun expand/2, Env),
  {{tuple, Meta, EArgs}, NewEnv, Expanded};

%% map
expand({map, Meta, Args}, Env) ->
  {EArgs, NewEnv, Expanded} = expand_list(Args, fun expand/2, Env),
  {{map, Meta, EArgs}, NewEnv, Expanded};

%% set

expand({set, Meta, Args}, Env) ->
  {EArgs, NewEnv, Expanded} = expand_list(Args, fun expand/2, Env),
  {{set, Meta, EArgs}, NewEnv, Expanded};


expand(Ast, Env) ->
  %% the default handler, which handles
  %% number, atom, identifier, strings(binary string and list string)
  {Ast, Env, false}.

%% Helpers

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

