%%
-module(ceiba_compiler).
-export([get_opt/1,
         string/2,
         quoted/2,
         file/1,
         file/2]).
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
string(Contents, File, Dest) ->
    Ast = ceiba:'string_to_ast!'(Contents, 1, File, []),
    quoted(Ast, File, Dest).

quoted(Forms, File) when is_binary(File) ->
    quoted(Forms, File, nil).
quoted(Forms, File, _Dest) ->
    Env = ceiba:env_for_eval([{line, 1}, {file, File}]),
    eval_forms(Forms, [], Env).

file(Relative) when is_binary(Relative) ->
    file(Relative, nil).
file(Relative, Dest) ->
    File = filename:absname(Relative),
    {ok, Bin} = file:read_file(File),
    string(ceiba_utils:characters_to_list(Bin),
           File,
           case Dest of
               nil -> Dest;
               _   -> filename:absname(Dest)
           end).

%% Evaluation

eval_forms(Forms, Vars, Env) ->
    case (?m(Env, module) == nil) andalso allows_fast_compilation(Forms) of
        true -> eval_compilation(Forms, Vars, Env);
        false -> code_loading_compilation(Forms, Vars, Env)
    end.

eval_compilation(Forms, Vars, Env) ->
    Binding = [{Key, Value} || {_Name, _Kind, Key, Value} <- Vars],
    {Result, _Binding, NewEnv, _Scope} = ceiba:eval_forms(Forms, Binding, Env),
    {Result, NewEnv}.

code_loading_compilation(Forms, Vars, #{line := _Line} = Env) ->
    Dict = [{{Name, Kind}, {Value, 0}} || {Name, Kind, Value, _} <- Vars],
    Scope = ceiba_env:env_to_scope_with_vars(Env, Dict),
    {Expr, _NewEnv, _NewScope} = ceiba:ast_to_abstract_format(Forms, Env, Scope),
    %% TODO
    io:format("~p~n", [Expr]).

allows_fast_compilation({'__block__', _, Exprs}) ->
    lists:all(fun allows_fast_compilation/1, Exprs);
allows_fast_compilation({defmodule, _, _}) -> true;
allows_fast_compilation(_) -> false.
