%%
-module(kapok_expand).
-export([macroexpand/2,
         macroexpand_1/2]).
-import(kapok_scanner, [token_meta/1, token_text/1]).
-include("kapok.hrl").

macroexpand(Ast, Env) ->
  {EAst, EEnv, Expanded} = macroexpand_1(Ast, Env),
  case Expanded of
    true -> macroexpand(EAst, EEnv);
    false -> {EAst, EEnv}
  end.

macroexpand_1({quote, Meta, {list, _, List}}, Env) ->
  {EList, EEnv} = list:mapfoldl(fun(X, E) -> macroexpand_1({quote, token_meta(X), X}, E) end,
                                Env,
                                List),
  {{list, Meta, EList}, EEnv, false};
macroexpand_1({quote, _, {C, _, _}} = Ast, Env) when ?is_dot_id(C) ->
  {Ast, Env, false};
%% quote an atom
macroexpand_1({quote, _, Arg}, Env) ->
  {Arg, Env, false};
macroexpand_1({backquote, _, {quote, Meta, Arg}}, Env) ->
  {EArg, EEnv, Expanded} = macroexpand_1({backquote, Meta, Arg}, Env),
  {{quote, Meta, EArg}, EEnv, Expanded};
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

