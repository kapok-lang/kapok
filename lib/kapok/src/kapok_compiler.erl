%% Compiler for kapok
-module(kapok_compiler).
-export([string_to_ast/4,
         'string_to_ast!'/4,
         ast_to_abstract_format/2,
         string/2,
         string/3,
         file/1,
         file/2,
         %% TODO
         eval_abstract_format/2
        ]).

-import(kapok_utils, [to_binary/1]).
-include("kapok.hrl").

%% Converts a given string (char list) into AST.

string_to_ast(String, StartLine, File, Options)
    when is_integer(StartLine), is_binary(File) ->
  case kapok_scanner:scan(String, StartLine, [{file, File}|Options]) of
    {ok, Tokens, _EndLocation} ->
      try kapok_parser:parse(Tokens) of
        {ok, Forms} -> {ok, Forms};
        {error, {Line, _, [Error, Token]}} -> {error, {Line, to_binary(Error), to_binary(Token)}}
      catch
        {error, {Line, _, [Error, Token]}} -> {error, {Line, to_binary(Error), to_binary(Token)}}
      end;
    {error, {Location, Module, ErrorDescription}, _Rest, _SoFar} ->
      {error, Location, Module, ErrorDescription}
  end.

'string_to_ast!'(String, StartLine, File, Options) ->
  case string_to_ast(String, StartLine, File, Options) of
    {ok, Forms} ->
      Forms;
    {error, Location, Module, ErrorDesc} ->
      {Line, _} = Location,
      kapok_error:parse_error(Line, File, Module, ErrorDesc)
  end.


%% Converts AST to erlang abstract format

ast_to_abstract_format(Ast, Env) ->
  {Expanded, EEnv} = kapok_expand:'expand-all'(Ast, Env),
  {Erl, TEnv} = kapok_translate:translate(Expanded, EEnv),
  io:format("~n after translate: ~p~n", [Erl]),
  %% TODO hard code exports
  [H | _T] = Erl,
  #{namespace := Namespace} = TEnv,
  T = {function,1,f,0,
          [{clause,1,[],[],
                   [{call,1,
                          {remote,1,{atom,1,io},{atom,1,format}},
                          [{string,1,"hello world!~n"},{nil,1}]}]}]},
%%{function,1,f,0,[{clause,1,[],[],[{nil,1}]}]},
  Erl0 = [H, kapok_namespace:export_forms(Namespace), T],
  io:format("~n after add exports: ~p~n", [Erl0]),
  {Erl0, TEnv}.

%% Compilation entry points.

string(Contents, File) when is_list(Contents), is_binary(File) ->
  string(Contents, File, nil).
string(Contents, File, DestFile) ->
  Ast = 'string_to_ast!'(Contents, 1, File, []),
  Env = kapok_env:env_for_eval([{line, 1}, {file, File}]),
  {Erl, _TEnv} = ast_to_abstract_format(Ast, Env),
  case compile:forms(Erl) of
    {ok, ModuleName, Binary} ->
      io:format("compiled module: ~p~n", [ModuleName]),
      case DestFile of
        nil ->
          {module, Module} = code:load_binary(ModuleName, binary_to_list(File), Binary),
          %% TODO hard code
          io:format("hot: ~p~n", [Module:f()]);
        _ ->
          ok = file:write_file(DestFile, Binary),
          {module, _Module} = code:load_binary(ModuleName, binary_to_list(File), Binary)
      end,
      io:format("done compiling ~p~n", [ModuleName]);
    {error, Errors, Warnings} ->
      io:format("~p~n", [Errors]),
      io:format("~p~n", [Warnings])
  end.

file(Relative) when is_binary(Relative) ->
  file(Relative, nil).
file(SourceFile, DestFile) ->
  {ok, Bin} = file:read_file(SourceFile),
  Contents = kapok_utils:characters_to_list(Bin),
  string(Contents, SourceFile, DestFile).

%% Evaluation

eval_abstract_format(Erl, #{vars := Vars} = Env) ->
  %% TODO add impl
  %% Erl is expression?
  {value, Value, NewBindings} = erl_eval:exprs(Erl, Vars),
  {Value, Env#{vars => NewBindings}}.

