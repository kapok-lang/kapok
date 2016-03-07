%% Translate Kapok AST to Erlang Abstract Format.
-module(kapok_translator).
-export([translate/2,
         translate_arg/3,
         translate_args/2,
         to_abstract_format/1]).
-import(kapok_scope, [mergev/2, mergef/2]).
-include("kapok.hrl").

%% Identifiers
translate({identifier, Meta, Identifier}, Scope) ->
  %% search scope to check whether identifier is a variable
  {{atom, ?line(Meta), list_to_atom(Identifier)}, Scope};

%% Operators
translate({Op, Meta, Number}, Scope) when Op == '+', Op == '-' ->
  {Erl, NewScope} = translate(Number, Scope),
  {{op, ?line(Meta), Op, Erl}, NewScope};

%% Local call
translate({list, Meta, [{identifier, _, _} = I|Args]}, Scope) ->
  Line = ?line(Meta),
  {TI, _} = translate(I, Scope),
  {TArgs, NewScope} = translate_args(Args, Scope),
  {{call, Line, TI, TArgs}, NewScope};

%%  Remote call
translate({list, Meta, [{dot, _, [Left, Right]}|Args]}, Scope) ->
  Line = ?line(Meta),
  {TLeft, _} = translate(Left, Scope),
  {TRight, _} = translate(Right, Scope),
  {TArgs, SA} = translate_args(Args, Scope),
  {{call, Line, {remote, Line, TLeft, TRight}, TArgs}, SA};
%% TODO

%% Containers

%% list
translate({list, _Meta, List}, Scope) ->
  translate_list(List, [], Scope);

%% binary
translate({binary, Meta, Args}, Scope) ->
  kapok_binary:translate(Meta, Args, Scope);

%% literals

%% number

%% integer
translate({number, Meta, Value}, Scope) when is_integer(Value) ->
  {{integer, ?line(Meta), Value}, Scope};
%% float
translate({number, Meta, Value}, Scope) when is_float(Value) ->
  {{float, ?line(Meta), Value}, Scope};

%% atom
translate({atom, Meta, Value}, Scope) ->
  {{atom, ?line(Meta), Value}, Scope};

%% list string
translate({list_string, Meta, Value}, Scope) ->
  {{string, ?line(Meta), binary_to_list(Value)}, Scope};

%% list
translate({literal_list, _Meta, List}, Scope) ->
  translate_list(List, [], Scope);

%% tuple
translate({tuple, Meta, Value}, Scope) ->
  {{tuple, ?line(Meta), Value}, Scope};

%% a list of ast
translate(List, Scope) when is_list(List) ->
  lists:mapfoldl(fun(E, S) -> translate(E, S) end,
                 Scope,
                 List).

%% Translate args

translate_arg(Arg, Acc, Scope)
    when is_number(Arg); is_atom(Arg); is_binary(Arg); is_pid(Arg); is_function(Arg) ->
  {TArg, _} = translate(Arg, Scope),
  {TArg, Acc};
translate_arg(Arg, Acc, Scope) ->
  {TArg, TAcc} = translate(Arg, mergef(Scope, Acc)),
  {TArg, mergev(Acc, TAcc)}.

translate_args(Args, #kapok_scope{context=match} = Scope) ->
  lists:mapfoldl(fun translate/2, Scope, Args);
translate_args(Args, Scope) ->
  lists:mapfoldl(fun(X, Acc) -> translate_arg(X, Acc, Scope) end,
                 Scope,
                 Args).

%% Helpers
translate_list([H|T], Acc, Scope) ->
  {Erl, NewScope} = translate(H, Scope),
  translate_list(T, [Erl|Acc], NewScope);
translate_list([], Acc, Scope) ->
  {build_list(Acc, {nil, 0}), Scope}.

build_list([H|T], Acc) ->
  build_list(T, {cons, 0, H, Acc});
build_list([], Acc) ->
  Acc.


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
