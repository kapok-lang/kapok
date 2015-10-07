%%
-module(ceiba_compiler).
-export([get_opt/1, string/2]).
-include("ceiba.hrl").

%% Pubilc API

get_opt(Key) ->
    Dict = ceiba_config:get(compiler_options),
    case lists:keyfind(Key, 1, Dict) of
        false -> false;
        {Key, Value} -> Value
    end.

%% Compilation entry points.

string(Contents, File) when is_list(Contents), is_binary(File) ->
    string(Contents, File, nil).
string(Contents, File, _) ->
    Ast = ceiba:string_to_ast(Contents, 1, File, []),
    Format = ceiba:ast_to_abstract_format(Ast),
    Format.
%    quoted(Forms, File, Dest).

%% ss() ->
%%     S = "(format 'str', (0xFF, 1235))",
%%     File = "test_file",
%%     string(S, 1, File, []).

