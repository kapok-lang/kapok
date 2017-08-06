%% Translate Kapok AST to Erlang Abstract Format.
-module(kapok_translate).
-export([translate/2,
         translate_def_arg/2,
         translate_def_args/2,
         translate_guard/2,
         map_vars/4,
         translate_body/3,
         quote/2,
         eval/2,
         construct_new_args/5,
         format_error/1]).
-import(kapok_scanner, [token_meta/1, token_text/1]).
-include("kapok.hrl").

%% a list of ast
translate(List, Env) when is_list(List) ->
  lists:mapfoldl(fun translate/2, Env, List);

%% literals

%% number
%% integer
translate({number, Meta, Number}, Env) when is_integer(Number) ->
  {{integer, ?line(Meta), Number}, Env};
%% float
translate({number, Meta, Number}, Env) when is_float(Number) ->
  {{float, ?line(Meta), Number}, Env};

%% Operators
translate({Op, Meta, Number}, Env) when ?is_op(Op) ->
  {Erl, TEnv} = translate(Number, Env),
  {{op, ?line(Meta), Op, Erl}, TEnv};

%% keyword and atom
translate({Category, Meta, Atom}, Env) when Category == keyword; Category == atom ->
  {{atom, ?line(Meta), Atom}, Env};

%% Identifiers
translate({identifier, Meta, Id}, #{context := Context} = Env) ->
  %% TODO if Id is a function name
  %% search env to check whether identifier is a variable
  {Name1, Env1} = case Context of
                    pattern ->
                      kapok_env:add_var(Meta, Env, Id);
                    let_pattern ->
                      kapok_env:add_let_var(Meta, Env, Id);
                    _ ->
                      case kapok_env:get_var(Meta, Env, Id) of
                        {ok, Name} -> {Name, Env};
                        error -> kapok_error:compile_error(Meta, ?m(Env, file),
                                                           "unable to resolve var: ~p", [Id])
                      end
                  end,
  {{var, ?line(Meta), Name1}, Env1};

%% binary string
translate({binary_string, _Meta, Binary}, Env) ->
  translate(Binary, Env);

%% list string
translate({list_string, Meta, Binary}, Env) ->
  {{string, ?line(Meta), binary_to_list(Binary)}, Env};

%% Containers

%% bitstring
translate({bitstring, Meta, Arg}, Env) ->
  kapok_trans_bitstring:translate(Meta, Arg, Env);

%% tuple
translate({tuple, Meta, Arg}, Env) ->
  {TArg, TEnv} = translate(Arg, Env),
  {build_tuple(Meta, TArg), TEnv};

%% map
translate({map, Meta, Arg}, Env) ->
  translate_map(Meta, Arg, Env);

%% set
translate({set, Meta, Arg}, Env) ->
  kapok_set:translate(Meta, Arg, Env);


%% list
translate({literal_list, _Meta, List}, Env) ->
  translate_list(List, Env);

translate({cons_list, _Meta, {Head, Tail}}, Env) ->
  translate_cons_list(Head, Tail, Env);

%% special forms
%% let
translate({list, Meta, [{identifier, _, 'let'}, {C, _, Args} | Body]}, Env) when ?is_list(C) ->
  Env1 = kapok_env:push_scope(Env),
  {TArgs, TEnv1} = translate_let_args(Args, Env1),
  {TBody, TEnv2} = translate_body(Meta, Body, TEnv1),
  BodyBlock = to_block(0, TBody),
  TEnv3 = kapok_env:pop_scope(TEnv2),
  {to_block(Meta, TArgs ++ [BodyBlock]), TEnv3};

%% do
translate({list, Meta, [{identifier, _, 'do'} | Exprs]}, Env) ->
  {TExprs, TEnv} = translate(Exprs, Env),
  {to_block(Meta, TExprs), TEnv};

%% case
translate({list, Meta, [{identifier, _, 'case'}, Expr, Clause | Left]}, Env) ->
  {TExpr, TEnv} = translate(Expr, Env),
  {TClause, TEnv1} = translate_case_clause(Clause, TEnv),
  {TLeft, TEnv2} = lists:mapfoldl(fun translate_case_clause/2, TEnv1, Left),
  {{'case', ?line(Meta), TExpr, [TClause | TLeft]}, TEnv2};

%% fn
translate({list, Meta, [{identifier, _, 'fn'}, {C, _, Id}, {number, _, Number}]}, Env)
    when ?is_local_id(C) ->
  translate_fn(Meta, Id, Number, Env);
translate({list, Meta, [{identifier, _, 'fn'}, {C1, _, Id1}, {C2, _, Id2}, {number, _, Number}]},
          Env)
    when ?is_local_id(C1), ?is_local_id(C2) ->
  translate_fn(Meta, Id1, Id2, Number, Env);
translate({list, Meta, [{identifier, _, 'fn'}, {identifier, _, Id}, {literal_list, _, _} = Args,
                        {list, _, [{identifier, _, 'when'} | _]} = Guard | Body]}, Env) ->
  translate_fn(Meta, Id, Args, Guard, Body, Env);
translate({list,
           Meta,
           [{identifier, _, 'fn'}, {identifier, _, Id}, {literal_list, _, _} = Args | Body]},
          Env) ->
  translate_fn(Meta, Id, Args, [], Body, Env);
translate({list, Meta, [{identifier, _, 'fn'}, {identifier, _, Id} | Exprs]}, Env) ->
  translate_fn(Meta, Id, Exprs, Env);
translate({list, Meta, [{identifier, _, 'fn'}, {literal_list, _, _} = Args,
                        {list, _, [{identifier, _, 'when'} | _]} = Guard | Body]}, Env) ->
  translate_fn(Meta, Args, Guard, Body, Env);
translate({list, Meta, [{identifier, _, 'fn'}, {literal_list, _, _} = Args | Body]}, Env) ->
  translate_fn(Meta, Args, [], Body, Env);
translate({list, Meta, [{identifier, _, 'fn'} | Exprs]}, Env) ->
  translate_fn(Meta, Exprs, Env);

%% send
translate({list, Meta, [{identifier, _, 'send'}, Pid, Message]}, Env) ->
  {TPid, TEnv} = translate(Pid, Env),
  {TMessage, TEnv1} = translate(Message, TEnv),
  {{op, ?line(Meta), '!', TPid, TMessage}, TEnv1};

%% receive
translate({list, Meta, [{identifier, _, 'receive'} | Clauses]}, Env) ->
  translate_receive(Meta, Clauses, Env);

%% try catch after
translate({list, Meta, [{identifier, _, 'try'} | Exprs]}, Env) ->
  translate_try(Meta, Exprs, Env);

%% Erlang specified forms

%% behaviour
translate({list, Meta, [{identifier, _, Form}, {Category, _, Id}]}, Env)
    when ?is_behaviour(Form), ?is_local_id(Category) ->
  translate_attribute(Meta, Form, Id, Env);

%% compile
translate({list, Meta, [{identifier, _, Form}, {Category, _, _} = Options]}, Env)
    when ?is_compile(Form), ?is_list(Category) ->
  {TOptions, TEnv} = kapok_compiler:ast(Options, Env),
  translate_attribute(Meta, Form, TOptions, TEnv);

%% file
translate({list, Meta, [{identifier, _, Form}, {C1, _, Binary}, {C2, _, Number}]}, Env)
    when ?is_file(Form), ?is_string(C1), ?is_number(C2) ->
  translate_attribute(Meta, Form, {Binary, Number}, Env);

%% wild attribute
translate({list, Meta, [{identifier, _, Form}, {C1, _, Attribute}, {C2, _, Value}]}, Env)
    when ?is_attribute(Form), ?is_local_id(C1), ?is_local_id(C2) ->
  translate_attribute(Meta, Attribute, Value, Env);

%% Local call
translate({list, Meta, [{identifier, Meta1, Id}| Args]}, Env) ->
  %% if it's a macro, expand it
  Arity = length(Args),
  {R1, Env1} = kapok_dispatch:find_local_macro(Meta, {Id, Arity}, Env),
  case R1 of
    {M1, F1, A1, P1} ->
      %% the same as macroexpand({list, ...}, Env1)
      NewArgs = kapok_translate:construct_new_args('expand', Arity, A1, P1, Args),
      {EAst, EEnv2} = kapok_dispatch:expand_macro_named(Meta, M1, F1, A1, NewArgs, Env1),
      translate(EAst, EEnv2);
    false ->
      {EArgs, Env2} = expand_list(Args, Env1),
      %% translate ast to erlang abstract format
      case kapok_env:get_var(Meta, Env2, Id) of
        {ok, Name} ->
          %% local variable
          {TF, TEnv2} = translate({identifier, Meta1, Name}, Env2),
          {TArgs, TEnv3} = translate_args(EArgs, TEnv2),
          translate_local_call(Meta, TF, TArgs, TEnv3);
        error ->
          {TArgs, TEnv2} = translate_args(EArgs, Env2),
          Arity1 = length(TArgs),
          FunArity = {Id, Arity1},
          case kapok_dispatch:find_local(FunArity, TEnv2) of
            {F2, A2, P2} ->
              translate_local_call(Meta, F2, A2, P2, Arity, TArgs, TEnv2);
            false ->
              %% check whether it's in imported functions/macros
              {R3, TEnv3} = kapok_dispatch:find_local_function(Meta, FunArity, TEnv2),
              case R3 of
                {M3, F3, A3, P3} ->
                  translate_remote_call(Meta, M3, F3, A3, P3, Arity, TArgs, TEnv3);
                _ ->
                  kapok_error:compile_error(Meta, ?m(TEnv3, file), "unknown local call: ~s",
                                            [FunArity])
              end
          end
      end
  end;
%%  Remote call
translate({list, Meta, [{dot, _, {Module, Fun}} | Args]}, Env) ->
  Arity = length(Args),
  {R1, Env1} = kapok_dispatch:find_remote_macro(Meta, Module, {Fun, Arity}, Env),
  case R1 of
    {M1, F1, A1, P1} ->
      %% the same as macroexpand({list, ...}, Env1)
      NewArgs = kapok_translate:construct_new_args('expand', Arity, A1, P1, Args),
      {EAst, EEnv2} = kapok_dispatch:expand_macro_named(Meta, M1, F1, A1, NewArgs, Env1),
      translate(EAst, EEnv2);
    false ->
      {EArgs, Env2} = expand_list(Args, Env1),
      {TArgs, TEnv2} = translate_args(EArgs, Env2),
      Arity1 = length(TArgs),
      FunArity = {Fun, Arity1},
      Namespace = ?m(TEnv2, namespace),
      case Module of
        Namespace ->
          %% call to local module
          case kapok_dispatch:find_local(FunArity, TEnv2) of
            {F2, A2, P2} -> translate_remote_call(Meta, Module, F2, A2, P2, Arity, TArgs, TEnv2);
            _ -> kapok_error:compile_error(Meta, ?m(TEnv2, file), "unknown remote call: ~s:~s",
                                           [Module, FunArity])
          end;
        _ ->
          {{M3, F3, A3, P3}, Env3} = kapok_dispatch:get_remote_function(Meta, Module, FunArity,
                                                                        TEnv2),
          translate_remote_call(Meta, M3, F3, A3, P3, Arity, TArgs, Env3)
      end
  end;
translate({list, Meta, [{list, _, _} = H | T]}, Env) ->
  {TH, TEnv} = translate(H, Env),
  {ET, EEnv1} = expand_list(T, TEnv),
  {TT, TEnv2} = translate(ET, EEnv1),
  translate_local_call(Meta, TH, TT, TEnv2);
translate({list, Meta, [_H | _T]} = Ast, Env) ->
  kapok_error:compile_error(Meta, ?m(Env, file), "invalid function call ~s", [token_text(Ast)]);
translate({list, _Meta, []}, Env) ->
  translate_list([], Env);

%% quote
translate({quote, Meta, Arg}, Env) ->
  %% TODO add unquote/unquote_splicing backquote pair checking
  translate(quote(Meta, Arg), Env);

%% backquote
%% The backquote is pushed down into the containers(list, cons-list, map, etc.) elements.
%% and then translated. We don't simply use backquote_level/unquote_level in macro_context
%% to indicate backquote/unquote environment in order to avoid always checking
%% backquote_level/unquote_level in other translate clauses for translating other types.

%% backquote a quote expression
translate({backquote, _, {quote, Meta, Arg}}, Env) ->
  {TC, TEnv} = translate({atom, Meta, 'quote'}, Env),
  {TMeta, TEnv1} = translate(quote(Meta, Meta), TEnv),
  {TArg, TEnv2} = translate({backquote, Meta, Arg}, TEnv1),
  {{tuple, ?line(Meta), [TC, TMeta, TArg]}, TEnv2};
%% embedded backquote
translate({backquote, _, {backquote, Meta, Arg}}, #{macro_context := Context} = Env) ->
  B = ?m(Context, backquote_level),
  Context1 = Context#{backquote_level => B + 1},
  {TC, TEnv1} = translate({atom, Meta, 'backquote'}, Env#{macro_context => Context1}),
  {TMeta, TEnv2} = translate(quote(Meta, Meta), TEnv1),
  {TArg, TEnv3} = translate({backquote, Meta, Arg}, TEnv2),
  {{tuple, ?line(Meta), [TC, TMeta, TArg]}, TEnv3#{macro_context => Context}};
%% backquote an unquote expression
translate({backquote, _, {unquote, Meta, Arg}}, #{macro_context := Context} = Env) ->
  B = ?m(Context, backquote_level),
  U = ?m(Context, unquote_level),
  if
    U == B ->
      {EArg, EEnv} = eval(Arg, Env),
      translate(EArg, EEnv);
    U < B ->
      {TC, TEnv}= translate({atom, Meta, 'unquote'}, Env),
      {TMeta, TEnv1} = translate(quote(Meta, Meta), TEnv),
      Context1 = Context#{unquote_level => U + 1},
      {TArg, TEnv2} = translate({backquote, Meta, Arg}, TEnv1#{macro_context => Context1}),
      {{tuple, ?line(Meta), [TC, TMeta, TArg]}, TEnv2#{macro_context => Context}};
    U > B ->
      kapok_error:compile_error(Meta, ?m(Env, file), "unquote doesn't match backquote")
  end;
%% backquote an unquote_splicing expression
translate({backquote, _, {unquote_splicing, Meta, Arg}}, #{macro_context := Context} = Env) ->
  B = ?m(Context, backquote_level),
  U = ?m(Context, unquote_level),
  if
    U == B ->
      {EArg, EEnv} = eval(Arg, Env),
      {TArg, TEnv} = translate(EArg, EEnv),
      {{evaluated_unquote_splicing, Meta, TArg}, TEnv};
    U < B ->
      {TC, TEnv} = translate({atom, Meta, 'unquote_splicing'}, Env),
      {TMeta, TEnv1} = translate(quote(Meta, Meta), TEnv),
      Context1 = Context#{unquote_level => U + 1},
      {TArg, TEnv2} = translate({backquote, Meta, Arg}, TEnv1#{macro_context => Context1}),
      {{tuple, ?line(Meta), [TC, TMeta, TArg]}, TEnv2#{macro_context => Context}};
    U > B ->
      kapok_error:compile_error(Meta, ?m(Env, file), "unquote_splicing doesn't match backquote")
  end;
%% backquote a list
translate({backquote, _, {Category, _, _} = Arg}, Env) when ?is_list(Category) ->
  translate_backquote_list(Arg, Env);
%% backquote a cons list
translate({backquote, _, {Category, Meta, {Head, Tail}}}, Env) when ?is_cons_list(Category) ->
  {TC, TEnv} = translate({atom, Meta, Category}, Env),
  {TMeta, TEnv1} = translate(quote(Meta, Meta), TEnv),
  {THead, TEnv2} = translate_list(Head, TEnv1),
  {TTail, TEnv3} = translate({backquote, token_meta(Tail), Tail}, TEnv2),
  {{tuple, ?line(Meta), [TC, TMeta, {tuple, ?line(Meta), [THead, TTail]}]}, TEnv3};
%% backquote a container but not list
translate({backquote, _, {Category, Meta, Args}}, Env)
    when Category == 'bitstring', is_list(Args);
         Category == 'tuple';
         Category == 'map';
         Category == 'set' ->
  {TC, TEnv} = translate({atom, Meta, Category}, Env),
  {TMeta, TEnv1} = translate(quote(Meta, Meta), TEnv),
  L = lists:map(fun(X) -> {backquote, token_meta(X), X} end, Args),
  {TArgs, TEnv2} = translate(L, TEnv1),
  {{tuple, ?line(Meta), [TC, TMeta, TArgs]}, TEnv2};
%% backquote a non-container type, e.g. an atom.
translate({backquote, Meta, Arg}, Env) ->
  translate({quote, Meta, Arg}, Env);

%% standalone unquote and unquote_splicing
translate({Category, Meta, _}, Env) when Category == unquote; Category == unquote_splicing ->
  kapok_error:compile_error(Meta, ?m(Env, file), "~s outside backquote", [Category]);
translate({evaluated_unquote_splicing, Meta, _}, Env) ->
  kapok_error:compile_error(Meta, ?m(Env, file), "unquote_splicing outside a list");

%% errors for function argument keywords
translate({Category, Meta} = Token, Env) when ?is_parameter_keyword(Category) ->
  Error = {parameter_keyword_outside_fun_args, {Token}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);

translate(Other, Env) ->
  {to_abstract_format(Other), Env}.

%% Converts specified code to erlang abstract format

to_abstract_format(Tree) when is_tuple(Tree) ->
  {tuple, 0, [to_abstract_format(X) || X <- tuple_to_list(Tree)]};
to_abstract_format([]) ->
  {nil, 0};
to_abstract_format(<<>>) ->
  {bin, 0, []};
to_abstract_format(Tree) when is_list(Tree) ->
  to_abstract_format_cons_1(Tree, []);
to_abstract_format(Tree) when is_atom(Tree) ->
  {atom, 0, Tree};
to_abstract_format(Tree) when is_integer(Tree) ->
  {integer, 0, Tree};
to_abstract_format(Tree) when is_float(Tree) ->
  {float, 0, Tree};
to_abstract_format(Tree) when is_binary(Tree) ->
  %% Note that our binaries are utf-8 encoded and we are converting
  %% to a list using binary_to_list. The reason for this is that Erlang
  %% considers a string in a binary to be encoded in latin1, so the bytes
  %% are not changed in any fashion.
  {bin, 0, [{bin_element, 0, {string, 0, binary_to_list(Tree)}, default, default}]};
to_abstract_format(Function) when is_function(Function) ->
  case (erlang:fun_info(Function, type) == {type, external}) andalso
    (erlang:fun_info(Function, env) == {env, []}) of
    true ->
      {module, Module} = erlang:fun_info(Function, module),
      {name, Name}     = erlang:fun_info(Function, name),
      {arity, Arity}   = erlang:fun_info(Function, arity),

      {'fun', 0, {function,
                  {atom, 0, Module},
                  {atom, 0, Name},
                  {integer, 0, Arity}}};
    false ->
      error(badarg)
  end;

to_abstract_format(Pid) when is_pid(Pid) ->
  abstract_format_remote_call(0, erlang, binary_to_term, [to_abstract_format(term_to_binary(Pid))]);
to_abstract_format(_Other) ->
  error(badarg).

to_abstract_format_cons_1([H|T], Acc) ->
  to_abstract_format_cons_1(T, [H|Acc]);
to_abstract_format_cons_1(Other, Acc) ->
  to_abstract_format_cons_2(Acc, to_abstract_format(Other)).

to_abstract_format_cons_2([H|T], Acc) ->
  to_abstract_format_cons_2(T, {cons, 0, to_abstract_format(H), Acc});
to_abstract_format_cons_2([], Acc) ->
  Acc.

abstract_format_remote_call(Line, Module, Function, Args) ->
  {call, Line, {remote, Line, {atom, Line, Module}, {atom, Line, Function}}, Args}.


%% Helpers

expand_list(List, Env) when is_list(List) ->
  lists:mapfoldl(fun kapok_macro:expand/2, Env, List).

eval({list, Meta, [{identifier, _, Id} | Args]} = Ast, Env) ->
  Arity = length(Args),
  {R, Env1} = kapok_dispatch:find_local_macro(Meta, {Id, Arity}, Env),
  case R of
    T when is_tuple(T) ->
      kapok_compiler:eval_ast(Ast, kapok_env:reset_macro_context(Env1));
    false ->
      {Ast, Env1}
  end;
eval({list, Meta, [{dot, _, {Module, Fun}} | Args]} = Ast, Env) ->
  Arity = length(Args),
  {R, Env1} = kapok_dispatch:find_remote_macro(Meta, Module, {Fun, Arity}, Env),
  case R of
    T when is_tuple(T) ->
      kapok_compiler:eval_ast(Ast, kapok_env:reset_macro_context(Env1));
    false ->
      {Ast, Env1}
  end;
eval(Ast, Env) ->
  {Ast, Env}.

%% Quotes.
%% atom
quote(Meta, Atom) when is_atom(Atom) ->
  {atom, Meta, Atom};
%% number
quote(Meta, Number) when is_number(Number) ->
  {number, Meta, Number};
%% list string and binary string
quote(Meta, Binary) when is_binary(Binary) ->
  {bitstring, Meta, {binary_string, Meta, Binary}};
%% tuple
quote(Meta, Tuple) when is_tuple(Tuple) ->
  {tuple, Meta, lists:map(fun(X) -> quote(Meta, X) end, tuple_to_list(Tuple))};
%% list
quote(Meta, List) when is_list(List) ->
  {literal_list, Meta, lists:map(fun(X) -> quote(Meta, X) end, List)};
%% map
quote(Meta, Map) when is_map(Map) ->
  {map, Meta, lists:reverse(lists:foldl(fun({K, V}, Acc) ->
                                            [quote(Meta, V), quote(Meta, K) | Acc]
                                        end,
                                        [],
                                        maps:to_list(Map)))}.

%% translate backquote list
translate_backquote_list({Category, Meta, List}, Env) ->
  {TL, TEnv} = lists:mapfoldl(fun(Ast, Env1) ->
                                  translate({backquote, token_meta(Ast), Ast}, Env1)
                              end,
                              Env,
                              List),
  translate_backquote_list(Category, Meta, lists:reverse(TL), [], [], TEnv).
translate_backquote_list(_Category, _Meta, [], [], Acc, Env) when is_tuple(Acc) ->
  {Acc, Env};
translate_backquote_list(Category, Meta, [], [], Acc, Env) when is_list(Acc) ->
  {TC, TEnv1} = translate({atom, Meta, Category}, Env),
  {TMeta, TEnv2} = translate(quote(Meta, Meta), TEnv1),
  TAst = build_tuple(Meta, [TC, TMeta, build_list(Acc)]),
  {TAst, TEnv2};
translate_backquote_list(Category, Meta, [], Atoms, Acc, Env) ->
  'build_list*'(Category, Meta, Atoms, Acc, Env);
translate_backquote_list(Category, Meta, [{evaluated_unquote_splicing, Meta1, Arg}|T], [], Acc,
                         Env) ->
  {TAst, TEnv} = build_append(Category, Meta1, Arg, Acc, Env),
  translate_backquote_list(Category, Meta, T, [], TAst, TEnv);
translate_backquote_list(Category, Meta, [{evaluated_unquote_splicing, Meta1, Arg}|T], Atoms, Acc,
                         Env) ->
  {TAst, TEnv} = 'build_list*'(Category, Meta1, Atoms, Acc, Env),
  {TAst1, TEnv1} = build_append(Category, Meta1, Arg, TAst, TEnv),
  translate_backquote_list(Category, Meta, T, [], TAst1, TEnv1);
translate_backquote_list(Category, Meta, [H|T], Atoms, Acc, Env) ->
  translate_backquote_list(Category, Meta, T, [H|Atoms], Acc, Env).

'build_list*'(Category, Meta, Atoms, Tail, Env) ->
  {TC, TEnv} = translate({atom, Meta, Category}, Env),
  {TMeta, TEnv1} = translate(quote(Meta, Meta), TEnv),
  {TListC, TEnv2} = translate({atom, Meta, 'list'}, TEnv1),
  {TDot, TEnv3} = translate({atom, Meta, 'dot'}, TEnv2),
  {TM, TEnv4} = translate({atom, Meta, 'kapok_macro'}, TEnv3),
  {TF, TEnv5} = translate({atom, Meta, 'list*'}, TEnv4),
  TListDot = build_tuple(Meta, [TDot, TMeta, build_tuple(Meta, [TM, TF])]),
  TAtoms = build_tuple(Meta, [TC, TMeta, build_list(Atoms)]),
  TTail = case Tail of
            _ when is_tuple(Tail) -> Tail;
            _ when is_list(Tail) -> build_tuple(Meta, [TC, TMeta, build_list(Tail)])
          end,
  TAst = build_tuple(Meta, [TListC, TMeta, build_list([TListDot, TAtoms, TTail])]),
  {TAst, TEnv5}.

build_append(Category, Meta, THead, Tail, Env) ->
  {TListC, TEnv} = translate({atom, Meta, 'list'}, Env),
  {TMeta, TEnv1} = translate(quote(Meta, Meta), TEnv),
  {TDot, TEnv2} = translate({atom, Meta, 'dot'}, TEnv1),
  {TM, TEnv3} = translate({atom, Meta, 'kapok_macro'}, TEnv2),
  {TF, TEnv4} = translate({atom, Meta, 'append'}, TEnv3),
  TAppend = build_tuple(Meta, [TDot, TMeta, build_tuple(Meta, [TM, TF])]),
  {TC, TEnv5} = translate({atom, Meta, Category}, TEnv4),
  TTail = case Tail of
            _ when is_tuple(Tail) -> Tail;
            _ when is_list(Tail) -> build_tuple(Meta, [TC, TMeta, build_list(Tail)])
          end,
  TAst = build_tuple(Meta, [TListC, TMeta, build_list([TAppend, THead, TTail])]),
  {TAst, TEnv5}.

%% translate map
build_map(Meta, TranslatedPairs) ->
  TFields = lists:map(fun({{_, Line, _} = K, V}) ->
                          {map_field_assoc, Line, K, V}
                      end,
                      TranslatedPairs),
  {map, ?line(Meta), TFields}.

translate_map(Meta, Args, #{context := Context} = Env) ->
  FieldType = case Context of
                pattern -> map_field_exact;
                _ -> map_field_assoc
              end,
  {TFields, TEnv} = build_map_field(Meta, FieldType, Args, Env),
  {{map, ?line(Meta), TFields}, TEnv}.

build_map_field(Meta, FieldType, Args, Env) ->
  build_map_field(Meta, FieldType, Args, [], Env).

build_map_field(_Meta, _FieldType, [], Acc, Env) ->
  {lists:reverse(Acc), Env};
build_map_field(Meta, _FieldType, [H], _Acc, Env) ->
  kapok_error:compile_error(Meta, ?m(Env, file), "unpaired values in map ~p", [H]);
build_map_field(Meta, FieldType, [K, V | Left], Acc, Env) ->
  {TK, TEnv} = kapok_translate:translate(K, Env),
  {TV, TEnv1} = kapok_translate:translate(V, TEnv),
  Field = {FieldType, ?line(kapok_scanner:token_meta(K)), TK, TV},
  build_map_field(Meta, FieldType, Left, [Field | Acc], TEnv1).

%% build tuple
build_tuple(Meta, TArg) when is_list(Meta) ->
  build_tuple(?line(Meta), TArg);
build_tuple(Line, TArg) when is_integer(Line) ->
  {tuple, Line, TArg}.

%% translate list
translate_list({Category, Meta, List}, Env) when ?is_list(Category) ->
  {TList, TEnv} = translate_list(List, Env),
  {{Category, Meta, TList}, TEnv};
translate_list(L, Env) when is_list(L) ->
  translate_list(L, [], Env).
translate_list([H|T], Acc, Env) ->
  {Erl, TEnv} = translate(H, Env),
  translate_list(T, [Erl|Acc], TEnv);
translate_list([], Acc, Env) ->
  {build_list_reversed(Acc), Env}.

build_list(L) ->
  build_list_reversed(lists:reverse(L)).
build_list_reversed(R) ->
  build_list_reversed(R, {nil, 0}).
build_list_reversed([H|T], Acc) ->
  build_list_reversed(T, {cons, 0, H, Acc});
build_list_reversed([], Acc) ->
  Acc.

%% translate cons_list
translate_cons_list(Head, Tail, Env) ->
  translate_cons_list(Head, [], Tail, Env).

translate_cons_list([], Acc, Tail, Env) ->
  {TTail, TEnv} = translate(Tail, Env),
  L = lists:foldl(fun(X, Acc1) ->
                      Line = erlang:element(2, X),
                      {cons, Line, X, Acc1}
                  end,
                  TTail,
                  Acc),
  {L, TEnv};
translate_cons_list([H | T], Acc, Tail, Env) ->
  {TH, TEnv} = translate(H, Env),
  translate_cons_list(T, [TH | Acc], Tail, TEnv).

%% translate let

translate_let_pattern(Arg, #{context := Context} = Env) ->
  {TArg, TEnv} = translate(Arg, Env#{context => let_pattern}),
  {TArg, TEnv#{context => Context}}.

translate_let_args(Args, Env) ->
  translate_let_args(Args, [], Env).
translate_let_args([], Acc, Env) ->
  {lists:reverse(Acc), Env#{context => nil}};
translate_let_args([H], _Acc, Env) ->
  Error = {let_odd_forms, {H}},
  kapok_error:form_error(kapok_scanner:token_meta(H), ?m(Env, file), ?MODULE, Error);
translate_let_args([P1, P2 | T], Acc, Env) ->
  {TP1, TEnv} = translate_let_pattern(P1, Env),
  {TP2, TEnv1} = translate(P2, TEnv),
  translate_let_args(T, [{match, ?line(kapok_scanner:token_meta(P1)), TP1, TP2} | Acc], TEnv1).

to_block(Meta, Exprs) when is_list(Meta) ->
  to_block(?line(Meta), Exprs);
to_block(Line, Exprs) when is_integer(Line) ->
  {block, Line, Exprs}.

%% special forms

translate_case_clause({list, Meta, []}, Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {empty_case_clause});
translate_case_clause({list, Meta, [_P]}, Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {missing_case_clause_body});
translate_case_clause({list, Meta, [P, {list, _, [{identifier, _, 'when'} | _]} = Guard | Body]},
                      Env) ->
  translate_case_clause(Meta, P, Guard, Body, Env);
translate_case_clause({list, Meta, [P | B]}, Env) ->
  translate_case_clause(Meta, P, [], B, Env);
translate_case_clause({C, Meta, _} = Ast, Env) when C /= list ->
  Error = {invalid_case_clause, {Ast}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error).

translate_case_clause(Meta, Pattern, Guard, Body, Env) ->
  Env1 = kapok_env:push_scope(Env),
  {TPattern, TEnv1} = translate_def_arg(Pattern, Env1),
  {TGuard, TEnv2} = translate_guard(Guard, TEnv1),
  {TBody, TEnv3} = translate_body(Meta, Body, TEnv2),
  TEnv4 = kapok_env:pop_scope(TEnv3),
  {{clause, ?line(Meta), [TPattern], TGuard, TBody}, TEnv4}.

translate_guard([], Env) ->
  {[], Env};
translate_guard({list, Meta, [{identifier, _, 'when'} | Body]}, Env) ->
  case Body of
    [] ->
      Error = {missing_case_clause_guard},
      kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);
    [H] ->
      translate_guard(H, nil, Env);
    [H | T] ->
      Error = {too_many_guards, {H, T}},
      kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error)
  end.
translate_guard({list, Meta, [{keyword, _, 'and'} | Left]}, 'and', Env) ->
  Error = {invalid_nested_and_or_in_guard, {'and', 'and', Left}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);
translate_guard({list, Meta, [{keyword, _, 'and'} | Left]}, Parent, Env) ->
  case Left of
    [E1, E2 | Tail] ->
      {TE1, TEnv} = translate_guard(E1, 'and', Env),
      {TE2, TEnv1} = translate_guard(E2, 'and', TEnv),
      {TLeft, TEnv2} = lists:mapfoldl(fun (X, E) -> translate_guard(X, 'and', E) end,
                                      TEnv1,
                                      Tail),
      L = [TE1, TE2 | TLeft],
      case Parent of
        nil -> {[L], TEnv2};
        'or' -> {L, TEnv2}
      end;
    [Tail] ->
      Error1 = {not_enough_operand_in_guard, {'and', Tail}},
      kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error1)
  end;
translate_guard({list, _, [{keyword, _, 'or'} | Left]}, nil, Env) ->
  lists:mapfoldl(fun (X, E) -> translate_guard(X, 'or', E) end, Env, Left);
translate_guard({list, Meta, [{keyword, _, 'or'} | Left]}, Parent, Env) ->
  Error = {invalid_nested_and_or_in_guard, {Parent, 'or', Left}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);
translate_guard(Other, nil, Env) ->
  {TOther, TEnv} = translate(Other, Env),
  %% as only expression in 'or'
  {[[TOther]], TEnv};
translate_guard(Other, 'or', Env) ->
  {TOther, TEnv} = translate(Other, Env),
  %% actually one expression in 'or'
  {[TOther], TEnv};
translate_guard(Other, _Parent, Env) ->
  translate(Other, Env).

%% translate fn

translate_fn(Meta, Exprs, Env) when is_list(Exprs) ->
  {Clauses, TEnv} = translate_fn_exprs(Exprs, Env),
  {{'fun', ?line(Meta), {clauses, Clauses}}, TEnv}.

translate_fn(Meta, Name, Arity, Env) when is_atom(Name), is_number(Arity) ->
  {{'fun', ?line(Meta), {function, Name, Arity}}, Env};
translate_fn(Meta, Name, Exprs, Env) when is_atom(Name), is_list(Exprs) ->
  {Clauses, TEnv} = translate_fn_exprs(Exprs, Env),
  {{'named_fun', ?line(Meta), Name, Clauses}, TEnv}.

translate_fn(Meta, Module, Name, Arity, Env)
    when is_atom(Module), is_atom(Name), is_number(Arity) ->
  {TModule, TEnv} = translate(Module, Env),
  {TName, TEnv1} = translate(Name, TEnv),
  {TArity, TEnv2} = translate(Arity, TEnv1),
  {{'fun', ?line(Meta), {function, TModule, TName, TArity}}, TEnv2};
translate_fn(Meta, Args, Guard, Body, Env)
    when is_tuple(Args), (is_list(Guard) orelse is_tuple(Guard)), is_list(Body) ->
  {Clause, TEnv} = translate_fn_clause(Meta, Args, Guard, Body, Env),
  {{'fun', ?line(Meta), {clauses, [Clause]}}, TEnv}.
translate_fn(Meta, Name, Args, Guard, Body, Env)
    when is_atom(Name), is_tuple(Args), (is_list(Guard) andalso is_tuple(Guard)), is_list(Body) ->
  {Clause, TEnv} = translate_fn_clause(Meta, Args, Guard, Body, Env),
  {{'named_fun', ?line(Meta), Name, [Clause]}, TEnv}.

translate_fn_exprs(Exprs, Env) when is_list(Exprs) ->
  check_fn_exprs(Exprs, Env),
  lists:mapfoldl(fun translate_fn_expr/2, Env, Exprs).

translate_fn_expr({list, Meta, [{literal_list, _, _} = Args,
                                {list, _, [{identifier, _, 'when'} | _]} = Guard | Body]},
                  Env) ->
  translate_fn_clause(Meta, Args, Guard, Body, Env);
translate_fn_expr({list, Meta, [{literal_list, _, _} = Args | Body]}, Env) ->
  translate_fn_clause(Meta, Args, [], Body, Env);
translate_fn_expr(Ast, Env) ->
  Error = {invalid_fn_expression, {Ast}},
  kapok_error:form_error(token_meta(Ast), ?m(Env, file), ?MODULE, Error).

translate_fn_clause(Meta, Args, Guard, Body, Env) ->
  Env1 = kapok_env:push_scope(Env),
  {TArgs, TEnv1} = translate_def_args(Args, Env1),
  {TGuard, TEnv2} = translate_guard(Guard, TEnv1),
  {TBody, TEnv3} = translate_body(Meta, Body, TEnv2),
  Clause = {clause, ?line(Meta), TArgs, TGuard, TBody},
  TEnv4 = kapok_env:pop_scope(TEnv3),
  {Clause, TEnv4}.

check_fn_exprs(Exprs, Env) when is_list(Exprs) ->
  lists:foldl(fun check_fn_expr_arity/2, {0, Env}, Exprs).
check_fn_expr_arity({list, Meta, [{literal_list, _, List} | _]}, {Last, Env}) ->
  Arity = length(List),
  case Last of
    0 ->
      {Arity, Env};
    Arity ->
      {Arity, Env};
    _ ->
      Error = {fn_clause_arity_mismatch, {Arity, Last}},
      kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error)
  end.

%% translate receive
translate_receive(Meta, [], Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {empty_receive_expr});
translate_receive(Meta, Clauses, Env) ->
  {Result, TEnv} = translate_receive(Meta, Clauses, [], Env),
  case Result of
    {TClauses, TExpr, TBody} when is_list(TClauses) ->
      {{'receive', ?line(Meta), TClauses, TExpr, TBody}, TEnv};
    TClauses when is_list(TClauses) ->
      {{'receive', ?line(Meta), TClauses}, TEnv}
  end.

translate_receive(_Meta, [], Acc, Env) ->
  {lists:reverse(Acc), Env};
translate_receive(Meta, [H], Acc, Env) ->
  case H of
    {list, Meta1, [{identifier, _, 'after'}, Expr | Body]} ->
      {TExpr, TEnv} = translate(Expr, Env),
      {TBody, TEnv1} = translate_body(Meta1, Body, TEnv),
      {{lists:reverse(Acc), TExpr, TBody}, TEnv1};
    _ ->
      {TClause, TEnv} = translate_case_clause(H, Env),
      translate_receive(Meta, [], [TClause | Acc], TEnv)
  end;
translate_receive(Meta, [H|T], Acc, Env) ->
  case H of
    {list, Meta1, [{identifier, _, 'after'}, _Expr | _Body]} ->
      kapok_error:form_error(Meta1, ?m(Env, file), ?MODULE, {after_clause_not_the_last});
    _ ->
      {TClause, TEnv} = translate_case_clause(H, Env),
      translate_receive(Meta, T, [TClause | Acc], TEnv)
  end.

%% translate try
translate_try(Meta, [], Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {empty_try_expr});
translate_try(Meta, [Expr|Left], Env) ->
  {TExpr, TEnv} = translate(Expr, Env),
  {CatchClauses, AfterBody, TEnv1} = translate_try_catch_after(Meta, Left, TEnv),
  %% notice that case clauses are always empty
  {{'try', ?line(Meta), [TExpr], [], CatchClauses, AfterBody}, TEnv1}.

translate_try_catch_after(Meta, [], Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {try_without_catch_or_after});
translate_try_catch_after(_Meta, [{list, Meta1, [{identifier, _, 'catch'} | CatchClauses]}], Env) ->
  {TCatchClauses, TEnv} = translate_catch_clauses(Meta1, CatchClauses, Env),
  {TCatchClauses, [], TEnv};
translate_try_catch_after(_Meta, [{list, Meta1, [{identifier, _, 'after'} | Body]}], Env) ->
  {TBody, TEnv} = translate_body(Meta1, Body, Env),
  {[], TBody, TEnv};
translate_try_catch_after(_Meta,
                          [{list, Meta1, [{identifier, _, 'catch'} | CatchClauses]},
                           {list, Meta2, [{identifier, _, 'after'} | Body]}],
                          Env)  ->
  {TCatchClauses, TEnv} = translate_catch_clauses(Meta1, CatchClauses, Env),
  {TBody, TEnv1} = translate_body(Meta2, Body, TEnv),
  {TCatchClauses, TBody, TEnv1};
translate_try_catch_after(Meta, Exprs, Env) ->
  Error = {invalid_catch_after_clause, {Exprs}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error).

translate_catch_clauses(Meta, [], Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {no_catch_clause});
translate_catch_clauses(_Meta, Clauses, Env) ->
  lists:mapfoldl(fun translate_catch_clause/2, Env, Clauses).

translate_catch_clause({list, Meta, []}, Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {empty_catch_clause});
translate_catch_clause({list, Meta, [E]}, Env) ->
  Error = {missing_catch_clause_body, {E}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);
translate_catch_clause({list, Meta, [E, {list, _, [{identifier, _, 'when'} | _]} = Guard | Body]},
                       Env) ->
  translate_catch_clause(Meta, E, Guard, Body, Env);
translate_catch_clause({list, Meta, [E | B]}, Env) ->
  translate_catch_clause(Meta, E, [], B, Env);
translate_catch_clause({C, Meta, _} = Ast, Env) when C /= list ->
  Error = {invalid_catch_clause, {Ast}},
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error).

translate_catch_clause(Meta, Exception, Guard, Body, Env) ->
  Env1 = kapok_env:push_scope(Env),
  {TException, TEnv1} = translate_exception(Exception, Env1),
  {TGuard, TEnv2} = translate_guard(Guard, TEnv1),
  {TBody, TEnv3} = translate_body(Meta, Body, TEnv2),
  TEnv4 = kapok_env:pop_scope(TEnv3),
  {{clause, ?line(Meta), [TException], TGuard, TBody}, TEnv4}.

translate_exception({list, Meta, [{Category, _, Atom} = Type, Pattern]}, Env)
    when Category == keyword; Category == atom ->
  case lists:any(fun(X) -> Atom == X end, ['throw', 'exit', 'error']) of
    false ->
      Error = {invalid_exception_type, {Atom}},
      kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, Error);
    true ->
      ok
  end,
  {TType, TEnv} = translate(Type, Env),
  {TPattern, TEnv1} = translate_def_arg(Pattern, TEnv),
  Line = ?line(Meta),
  {{tuple, Line, [TType, TPattern, {var, Line, '_'}]}, TEnv1};
translate_exception(Pattern, Env) ->
  {TPattern, TEnv} = translate_def_arg(Pattern, Env),
  Line = ?line(token_meta(Pattern)),
  {{tuple, Line, [{keyword, Line, 'throw'}, TPattern, {var, Line, '_'}]}, TEnv}.

%% translate attribute

translate_attribute(Meta, A, T, Env) ->
  {{attribute,?line(Meta), A, T}, Env}.

%% translate local call
translate_local_call(Meta, F, A, P, Arity, TArgs, Env) ->
  {TF, TEnv} = translate(F, Env),
  TArgs1 = construct_new_args('translate', Arity, A, P, TArgs),
  translate_local_call(Meta, TF, TArgs1, TEnv).
translate_local_call(Meta, TF, TArgs, Env) ->
  {{call, ?line(Meta), TF, TArgs}, Env}.

%% translate remote call
translate_remote_call(Meta, M, F, A, P, Arity, TArgs, Env) ->
  TArgs1 = construct_new_args('translate', Arity, A, P, TArgs),
  translate_remote_call(Meta, M, F, TArgs1, Env).
translate_remote_call(Meta, M, F, TArgs, Env) ->
  {TM, TEnv} = translate(M, Env),
  {TF, TEnv1} = translate(F, TEnv),
  Line = ?line(Meta),
  {{call, Line, {remote, Line, TM, TF}, TArgs}, TEnv1}.

construct_new_args(Context, Arity, NewArity, ParaType, Args) ->
  case (ParaType == rest) andalso (Arity >= NewArity) of
    true ->
      {NormalParas, RestPara} = lists:split(NewArity-1, Args),
      case Context of
        'expand' -> NormalParas ++ [RestPara];
        'translate' -> NormalParas ++ [build_list(RestPara)]
      end;
    false ->
      Args
  end.

map_vars(Meta, TMapArg, TKeyParameters, Env) ->
  {TM, TEnv} = translate({atom, Meta, 'maps'}, Env),
  {TF, TEnv1} = translate({atom, Meta, 'get'}, TEnv),
  Line = ?line(Meta),
  L = lists:foldl(fun({TId, TDefault}, Acc) ->
                      Call = {call, Line, {remote, Line, TM, TF},
                              [var_to_keyword(TId), TMapArg, TDefault]},
                      C = {match, ?line(Meta), TId, Call},
                      [C | Acc]
                  end,
                  [],
                  TKeyParameters),
  {lists:reverse(L), TEnv1}.

var_to_keyword({var, Line, Id}) ->
  {atom, Line, Id}.

%% Translate arguments

translate_def_arg(Arg, #{context := Context} = Env) ->
  {TArg, TEnv} = translate(Arg, Env#{context => pattern}),
  {TArg, TEnv#{context => Context}}.

translate_def_args({literal_list, _, Args}, Env) ->
  lists:mapfoldl(fun translate_def_arg/2, Env, Args);
translate_def_args(Args, Env) when is_list(Args) ->
  lists:mapfoldl(fun translate_def_arg/2, Env, Args).

translate_arg(Arg, Env) ->
  translate(Arg, Env).

translate_args({Category, _, Args}, Env) when ?is_list(Category) ->
  translate_args(Args, Env);
translate_args(Args, Env) when is_list(Args) ->
  {L, TEnv} = translate_args(Args, {[], []}, {normal, [], []}, Env),
  case lists:reverse(L) of
    [{normal, _, TNormalArgs}] ->
      {TNormalArgs, TEnv};
    [{normal, _, TNormalArgs}, {keyword, Meta, Pairs}] ->
      MapArg = build_map(Meta, Pairs),
      {TNormalArgs ++ [MapArg], TEnv}
  end.

translate_args([], {_Keys, Acc}, {Previous, Meta, Args}, Env) ->
  {[{Previous, Meta, lists:reverse(Args)} | Acc], Env};

translate_args([{keyword, Meta, _} = Token], _, {_Previous, _Meta, _Args}, Env) ->
  kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {dangling_keyword, {Token}});
translate_args([{keyword, Meta, Atom} = Keyword, Expr | T], {Keys, Acc}, {normal, Meta1, Args},
               Env) ->
  case ordsets:is_element(Atom, Keys) of
    true -> kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {duplicate_keyword, {Keyword}});
    false -> ok
  end,
  {TKeyword, TEnv} = translate_arg(Keyword, Env),
  {TExpr, TEnv1} = translate_arg(Expr, TEnv),
  Keys1 = ordsets:add_element(Atom, Keys),
  Acc1 = [{normal, Meta1, lists:reverse(Args)} | Acc],
  translate_args(T, {Keys1, Acc1}, {keyword, Meta, [{TKeyword, TExpr}]}, TEnv1);
translate_args([{keyword, Meta, Atom} = Keyword, Expr | T], {Keys, Acc}, {keyword, Meta1, Args},
               Env) ->
  case ordsets:is_element(Atom, Keys) of
    true -> kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {duplicate_keyword, {Keyword}});
    false -> ok
  end,
  {TKeyword, TEnv} = translate_arg(Keyword, Env),
  {TExpr, TEnv1} = translate_arg(Expr, TEnv),
  Keys1 = ordsets:add_element(Atom, Keys),
  translate_args(T, {Keys1, Acc}, {keyword, Meta1, [{TKeyword, TExpr} | Args]}, TEnv1);
translate_args([H | T], Acc, {normal, Meta1, Args}, Env) ->
  {TH, TEnv} = translate_arg(H, Env),
  translate_args(T, Acc, {normal, Meta1, [TH | Args]}, TEnv);
translate_args([H | _T], _, {keyword, _Meta1, _Args}, Env) ->
  kapok_error:form_error(token_meta(H), ?m(Env, file), ?MODULE, {missing_key_for_argument, {H}}).

%% translate body
translate_body(Meta, Body, Env) ->
  case Body of
    [] ->
      kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {missing_body});
    _ ->
      translate(Body, Env)
  end.

%% Error

format_error({let_odd_forms, {Form}}) ->
  io_lib:format("let requires an even number of forms in binding, unpaired form: ~p~n", [Form]);
format_error({empty_case_clause}) ->
  io_lib:format("case clause is empty");
format_error({missing_case_clause_body}) ->
  io_lib:format("case clause body is missing");
format_error({invalid_case_clause, {Ast}}) ->
  io_lib:format("invalid case clause ~w", [Ast]);
format_error({missing_case_clause_guard}) ->
  io_lib:format("case clause guard is missing");
format_error({too_many_guards, {First, Left}}) ->
  io_lib:format("too many arguments for when, please use and/or for "
                "guard tests or sequences: ~w, ~w",
                [First, Left]);
format_error({invalid_nested_and_or_in_guard, {Parent, Id, Left}}) ->
  io_lib:format("invalid nested and/or in guard, parent: ~p, current: ~p, args: ~w",
                [Parent, Id, Left]);
format_error({not_enough_operand_in_guard, {Op, Operand}}) ->
  io_lib:format("not enough operand for ~p, given: ~w", [Op, Operand]);
format_error({invalid_fn_expression, {Expr}}) ->
  io_lib:format("invalid fn expression ~w", [Expr]);
format_error({fn_clause_arity_mismatch, {Arity, Last}}) ->
  io_lib:format("fn clause arity ~B mismatch with last clause arity ~B~n", [Arity, Last]);
format_error({empty_receive_expr}) ->
  io_lib:format("empty receive expression", []);
format_error({after_clause_not_the_last}) ->
  io_lib:format("after clause should be the last expression of receive or try catch", []);
format_error({empty_try_expr}) ->
  io_lib:format("empty try expression", []);
format_error({try_without_catch_or_after}) ->
  io_lib:format("try expression without catch or after clause", []);
format_error({invalid_catch_after_clause, {Exprs}}) ->
  io_lib:format("invalid catch and after clause in try: ~w", [Exprs]);
format_error({missing_catch_clause}) ->
  io_lib:format("catch clause is missing", []);
format_error({empty_catch_clause}) ->
  io_lib:format("catch clause is empty", []);
format_error({missing_catch_clause_body}) ->
  io_lib:format("catch clause body is missing", []);
format_error({invalid_catch_clause, {Ast}}) ->
  io_lib:format("invalid catch clause: ~w", [Ast]);
format_error({invalid_exception_type, {Type}}) ->
  io_lib:format("invalid exception type: ~p", [Type]);
format_error({parameter_keyword_outside_fun_args, {Token}}) ->
  io_lib:format("invalid ~s outside the arguments of a function definition", [token_text(Token)]);
format_error({missing_body}) ->
  io_lib:format("body is missing", []);
format_error({dangling_keyword, {Token}}) ->
  io_lib:format("dangling ~s without argument", [token_text(Token)]);
format_error({duplicate_keyword, {Token}}) ->
  io_lib:format("duplicate keyword ~s", [token_text(Token)]);
format_error({missing_key_for_argument, {Token}}) ->
  io_lib:format("missing key for argument ~s", [token_text(Token)]).
