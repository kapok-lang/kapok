%% Translate Kapok AST to Erlang Abstract Format.
-module(kapok_trans).
-export([translate/2,
         map_vars/4,
         translate_def_arg/2,
         translate_def_args/2,
         translate_guard/2,
         translate_body/3,
         quote/2,
         eval/2,
         construct_new_args/5,
         format_error/1]).
-import(kapok_scanner, [token_meta/1, token_text/1]).
-import(kapok_trans_container, [build_tuple/2,
                                build_map_from/2,
                                build_list/1,
                                translate_tuple/3,
                                translate_map/3,
                                translate_set/3,
                                translate_list/2,
                                translate_cons_list/3
                               ]).
-import(kapok_trans_special_form, [translate_attribute/4,
                                   translate_let/4,
                                   translate_do/3,
                                   translate_case/5,
                                   translate_fn/3,
                                   translate_fn/4,
                                   translate_fn/5,
                                   translate_fn/6,
                                   translate_send/4,
                                   translate_receive/3,
                                   translate_try/3
                                  ]).
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
  translate_tuple(Meta, Arg, Env);

%% map
translate({map, Meta, Arg}, Env) ->
  translate_map(Meta, Arg, Env);

%% set
translate({set, Meta, Arg}, Env) ->
  translate_set(Meta, Arg, Env);

%% list
translate({literal_list, _Meta, List}, Env) ->
  translate_list(List, Env);

translate({cons_list, _Meta, {Head, Tail}}, Env) ->
  translate_cons_list(Head, Tail, Env);

%% special forms
%% let
translate({list, Meta, [{identifier, _, 'let'}, {C, _, Args} | Body]}, Env) when ?is_list(C) ->
  translate_let(Meta, Args, Body, Env);

%% do
translate({list, Meta, [{identifier, _, 'do'} | Exprs]}, Env) ->
  translate_do(Meta, Exprs, Env);

%% case
translate({list, Meta, [{identifier, _, 'case'}, Expr, Clause | Left]}, Env) ->
  translate_case(Meta, Expr, Clause, Left, Env);

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
  translate_send(Meta, Pid, Message, Env);

%% receive
translate({list, Meta, [{identifier, _, 'receive'} | Clauses]}, Env) ->
  translate_receive(Meta, Clauses, Env);

%% try catch after block
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
  %% translate ast to erlang abstract format
  case kapok_env:get_var(Meta, Env, Id) of
    {ok, Name} ->
      %% local variable
      {TF, TEnv1} = translate({identifier, Meta1, Name}, Env),
      {TArgs, TEnv2} = translate_args(Args, TEnv1),
      translate_local_call(Meta, TF, TArgs, TEnv2);
    error ->
      {TArgs, TEnv1} = translate_args(Args, Env),
      Arity = length(TArgs),
      FunArity = {Id, Arity},
      case kapok_dispatch:find_local(FunArity, TEnv1) of
        {F2, A2, P2} ->
          translate_local_call(Meta, F2, A2, P2, Arity, TArgs, TEnv1);
        false ->
          %% check whether it's in imported functions/macros
          {R3, TEnv2} = kapok_dispatch:find_local_function(Meta, FunArity, TEnv1),
          case R3 of
            {M3, F3, A3, P3} ->
              translate_remote_call(Meta, M3, F3, A3, P3, Arity, TArgs, TEnv2);
            _ ->
              kapok_error:compile_error(Meta, ?m(TEnv2, file), "unknown local call: ~s", [FunArity])
          end
      end
  end;
%%  Remote call
translate({list, Meta, [{dot, _, {Module, Fun}} | Args]}, Env) ->
  {TArgs, TEnv1} = translate_args(Args, Env),
  Arity = length(Args),
  FunArity = {Fun, Arity},
  Namespace = ?m(TEnv1, namespace),
  case Module of
    Namespace ->
      %% call to local module
      case kapok_dispatch:find_local(FunArity, TEnv1) of
        {F2, A2, P2} -> translate_remote_call(Meta, Module, F2, A2, P2, Arity, TArgs, TEnv1);
        _ -> kapok_error:compile_error(Meta, ?m(TEnv1, file),
                                       "unknown remote call: ~s:~s", [Module, FunArity])
      end;
    _ ->
      {{M3, F3, A3, P3}, Env2} = kapok_dispatch:get_remote_function(Meta, Module, FunArity, TEnv1),
      translate_remote_call(Meta, M3, F3, A3, P3, Arity, TArgs, Env2)
  end;
translate({list, Meta, [{list, _, _} = H | T]}, Env) ->
  {TH, TEnv1} = translate(H, Env),
  {TT, TEnv2} = translate(T, TEnv1),
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
translate_backquote_list(Category, Meta, [], Atoms, Acc, Env) when is_tuple(Acc) ->
  'build_macro_list*'(Category, Meta, Atoms, Acc, Env);
translate_backquote_list(Category, Meta, [], Atoms, Acc, Env) when is_list(Acc) ->
  translate_backquote_list(Category, Meta, [], [], lists:append(Atoms, Acc), Env);
translate_backquote_list(Category, Meta, [{evaluated_unquote_splicing, Meta1, Arg}|T], Atoms, Acc, Env) ->
  {TAst, TEnv} = translate_backquote_list(Category, Meta, [], Atoms, Acc, Env),
  {TAst1, TEnv1} = build_macro_append(Meta1, Arg, TAst, TEnv),
  translate_backquote_list(Category, Meta, T, [], TAst1, TEnv1);
translate_backquote_list(Category, Meta, [H|T], Atoms, Acc, Env) ->
  translate_backquote_list(Category, Meta, T, [H|Atoms], Acc, Env).

'build_macro_list*'(Category, Meta, Atoms, Tail, Env) when is_tuple(Tail) ->
  {TC, TEnv} = translate({atom, Meta, Category}, Env),
  {TMeta, TEnv1} = translate(quote(Meta, Meta), TEnv),
  {TListC, TEnv2} = translate({atom, Meta, 'list'}, TEnv1),
  {TDot, TEnv3} = translate({atom, Meta, 'dot'}, TEnv2),
  {TM, TEnv4} = translate({atom, Meta, 'kapok_macro'}, TEnv3),
  {TF, TEnv5} = translate({atom, Meta, 'list*'}, TEnv4),
  TListDot = build_tuple(Meta, [TDot, TMeta, build_tuple(Meta, [TM, TF])]),
  TAtoms = build_tuple(Meta, [TC, TMeta, build_list(Atoms)]),
  TAst = build_tuple(Meta, [TListC, TMeta, build_list([TListDot, TAtoms, Tail])]),
  {TAst, TEnv5}.

build_macro_append(Meta, THead, Tail, Env) ->
  {TListC, TEnv} = translate({atom, Meta, 'list'}, Env),
  {TMeta, TEnv1} = translate(quote(Meta, Meta), TEnv),
  {TDot, TEnv2} = translate({atom, Meta, 'dot'}, TEnv1),
  {TM, TEnv3} = translate({atom, Meta, 'kapok_macro'}, TEnv2),
  {TF, TEnv4} = translate({atom, Meta, 'append'}, TEnv3),
  TAppend = build_tuple(Meta, [TDot, TMeta, build_tuple(Meta, [TM, TF])]),
  TAst = build_tuple(Meta, [TListC, TMeta, build_list([TAppend, THead, Tail])]),
  {TAst, TEnv4}.

%% special forms

%% translate attribute

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
      MapArg = build_map_from(Meta, Pairs),
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

%% translate guard
translate_guard([], Env) ->
  {[], Env};
translate_guard({list, Meta, [{identifier, _, 'when'} | Body]}, Env) ->
  case Body of
    [] ->
      Error = {missing_guard},
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

%% translate body
translate_body(Meta, Body, Env) ->
  case Body of
    [] ->
      kapok_error:form_error(Meta, ?m(Env, file), ?MODULE, {missing_body});
    _ ->
      translate(Body, Env)
  end.

%% Error

format_error({parameter_keyword_outside_fun_args, {Token}}) ->
  io_lib:format("invalid ~s outside the arguments of a function definition", [token_text(Token)]);
format_error({missing_body}) ->
  io_lib:format("body is missing", []);
format_error({dangling_keyword, {Token}}) ->
  io_lib:format("dangling ~s without argument", [token_text(Token)]);
format_error({duplicate_keyword, {Token}}) ->
  io_lib:format("duplicate keyword ~s", [token_text(Token)]);
format_error({missing_key_for_argument, {Token}}) ->
  io_lib:format("missing key for argument ~s", [token_text(Token)]);
format_error({missing_guard}) ->
  io_lib:format("case clause guard is missing");
format_error({too_many_guards, {First, Left}}) ->
  io_lib:format("too many arguments for when, please use and/or for "
                "guard tests or sequences: ~w, ~w",
                [First, Left]);
format_error({invalid_nested_and_or_in_guard, {Parent, Id, Left}}) ->
  io_lib:format("invalid nested and/or in guard, parent: ~p, current: ~p, args: ~w",
                [Parent, Id, Left]);
format_error({not_enough_operand_in_guard, {Op, Operand}}) ->
  io_lib:format("not enough operand for ~p, given: ~w", [Op, Operand]).
