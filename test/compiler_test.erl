-module(compiler_test).
-include_lib("eunit/include/eunit.hrl").

warn(Warnings) ->
    io:fwrite("~s~n", [Warnings]).
warn(Format, Args) ->
    io:fwrite(Format, Args).

erl_to_abstract_format(Contents, Type) ->
    case erl_scan:string(Contents) of
        {ok, Tokens, _EndLocation} ->
            case Type of
                term ->
                    {ok, Term} = erl_parse:parse_term(Tokens),
                    Term;
                exprs ->
                    {ok, ExprList} = erl_parse:parse_exprs(Tokens),
                    ExprList;
                form ->
                    {ok, Form} = erl_parse:parse_form(Tokens),
                    Form;
                _ ->
                    throw({"invalid type of erl tokens: ~s~n", [Type]})
            end;
        {error, ErrorInfo, ErrorLocation}->
            ?assert(false),
            throw({"scan error, location: ~w, error: ~s~n", [ErrorLocation, ErrorInfo]})
    end.

eval_erlang_exprs(Exprs) ->
    Parsed = erl_to_abstract_format(Exprs, exprs),
    io:format("~nerlang exprs: ~w~n", [Parsed]),
    erl_eval:expr_list(Parsed, []).

eval_ceiba_exprs(Exprs) ->
    Parsed = ceiba_compiler:string(Exprs, <<"file">>),
    io:format("ceiba  exprs: ~w~n", [Parsed]),
    erl_eval:expr_list(Parsed, []).

list_test() ->
    Output1 = eval_erlang_exprs("[1 | [2]]."),
    Output2 = eval_erlang_exprs("[1, 2]."),
    Output3 = eval_ceiba_exprs("(1 2)"),
    Output4 = eval_ceiba_exprs("[1 2]"),
    ?assertEqual(Output3, Output1),
    ?assertEqual(Output3, Output2),
    ?assertEqual(Output4, Output3).

binary_test() ->
    Output1 = eval_erlang_exprs("<<256:8/big-unsigned-integer-unit:1>>."),
    Output2 = eval_ceiba_exprs("<<(256 (:size 8) :big :unsigned :integer (:unit 1))>>"),
    ?assertEqual(Output1, Output2).


string_test() ->
    Format = ceiba_compiler:string("(1 2)", <<"file">>),
    Expect = [{cons,0,{integer,1,1},{cons,0,{integer,1,2},{nil,0}}}],
    ?assertEqual(Expect, Format),
    F1 = {function,3,f,0,
             [{clause,3,[],[],
                      Format}]},
    F = [{attribute,1,module,test}, {attribute,2,export,[{f,0}]}, F1],
    case compile:forms(F, [return]) of
        {ok, Module, Binary, Warnings} ->
            warn(Warnings),
            {module, Module} = code:load_binary(Module, nil, Binary),
            ?assertEqual([1, 2], Module:f());
        {error, Errors, Warnings} ->
            ?assert(false),
            warn(Warnings),
            warn(Errors)
    end.
