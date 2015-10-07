-module(compiler_test).
-include_lib("eunit/include/eunit.hrl").

warn(Warnings) ->
    io:fwrite("~w~n", [Warnings]).

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
