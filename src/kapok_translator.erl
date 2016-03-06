%% Translate Kapok AST to Erlang Abstract Format.
-module(kapok_translator).

-export([translate/2,
         translate_arg/3,
         translate_args/2]).
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
%% tuple
translate({tuple, Meta, Value}, Scope) ->
    {{tuple, ?line(Meta), Value}, Scope};

%% a list of ast
translate(List, Scope) when is_list(List) ->
    lists:mapfoldl(fun(E, S) -> translate(E, S) end,
                   Scope,
                   List).

%% Translate args

translate_arg(Arg, Acc, Scope) when is_number(Arg); is_atom(Arg); is_binary(Arg); is_pid(Arg); is_function(Arg) ->
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

