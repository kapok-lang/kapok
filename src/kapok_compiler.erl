%% Compiler for kapok
-module(kapok_compiler).
-export([string_to_ast/4,
         'string_to_ast!'/4,
         ast_to_abstract_format/2,
         compile/2,
         compile_file/1,
         compile_file/2,
         compile_file_and_eval/1,
         %% TODO
         eval_ast/2,
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
      Line = kapok_scanner:location_line(Location),
      kapok_error:parse_error(Line, File, Module, ErrorDesc)
  end.


%% Converts AST to erlang abstract format

ast_to_abstract_format(Ast, Env) ->
  {Expanded, EEnv} = kapok_expand:'expand-all'(Ast, Env),
  io:format("~n after expand: ~p~n", [Expanded]),
  {Erl, TEnv} = kapok_translate:translate(Expanded, EEnv),
  io:format("~n after translate: ~p~n", [Erl]),
  {Erl, TEnv}.

%% Compilation entry points.

compile(Contents, File) when is_list(Contents) ->
  Ast = 'string_to_ast!'(Contents, 1, File, []),
  Env = kapok_env:env_for_eval([{line, 1}, {file, File}]),
  {Erl, _TEnv} = ast_to_abstract_format(Ast, Env),
  Erl.

dest_to_compile(SourceFile) ->
  AbsName = filename:absname(SourceFile),
  DirName = filename:dirname(AbsName),
  BaseName = filename:basename(AbsName, ?SOURCE_FILE_SUFFIX),
  filename:join([DirName, BaseName, ?BEAM_FILE_SUFFIX]).

compile_file(Relative) when is_binary(Relative) ->
  SourceFile = filename:absname(Relative),
  compile_file(SourceFile, dest_to_compile(SourceFile)).
compile_file(SourceFile, DestFile) ->
  {ok, Bin} = file:read_file(SourceFile),
  Contents = kapok_utils:characters_to_list(Bin),
  Erl = compile(Contents, SourceFile),
  Bytes = io_lib:format("~p", [Erl]),
  ok = file:write_file(DestFile, Bytes).

compile_file_and_eval(Relative) when is_binary(Relative) ->
  compile_file(Relative).
%% eval compilation.
%%eval_ast(Ast, Env).

%% Evaluation

eval_ast(Ast, Env) ->
  %% TODO add impl
  {Erl, NewEnv} = ast_to_abstract_format(Ast, Env),
  %% TODO translate module definition

  eval_abstract_format(Erl, NewEnv).

eval_abstract_format(Erl, #{vars := Vars} = Env) ->
  %% TODO add impl
  %% Erl is expression?
  {value, Value, NewBindings} = erl_eval:exprs(Erl, Vars),
  {Value, Env#{vars => NewBindings}}.

