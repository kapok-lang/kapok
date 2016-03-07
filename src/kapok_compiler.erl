%% Compiler for kapok
-module(kapok_compiler).
-export([string/2,
         file/1,
         file/2]).
-include("kapok.hrl").

%% Converts a given string (char list) into AST.

string_to_ast(String, StartLine, File, Options)
    when is_integer(StartLine), is_binary(File) ->
  case kapok_scanner:scan(String, StartLine, [{file, File}|Options]) of
    {ok, Tokens, _EndLocation} ->
      try kapok_parser:parse(Tokens) of
          {ok, Forms} -> {ok, Forms};
          {error, {Line, _, [Error, Token]}} ->
          {error, {Line, to_binary(Error), to_binary(Token)}}
      catch
        {error, {Line, _, [Error, Token]}} ->
          {error, {Line, to_binary(Error), to_binary(Token)}}
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

to_binary(List) when is_list(List) -> unicode:characters_to_binary(List);
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8).

%% Converts AST to erlang abstract format

ast_to_abstract_format(Ast, Env) ->
  ast_to_abstract_format(Ast, Env, kapok_env:env_to_scope(Env)).

ast_to_abstract_format(Ast, Env, Scope) ->
  {Expanded, NewEnv} = kapok_expand:expand(Ast, Env),
  {Erl, NewScope} = kapok_translator:translate(Expanded, Scope),
  {Erl, NewEnv, NewScope}.

%% Compilation entry points.

file(Relative) when is_binary(Relative) ->
  file(Relative, nil).
file(Relative, Dest) ->
  File = filename:absname(Relative),
  {ok, Bin} = file:read_file(File),
  string(kapok_utils:characters_to_list(Bin),
         File,
         case Dest of
           nil -> Dest;
           _   -> filename:absname(Dest)
         end).

string(Contents, File) when is_list(Contents), is_binary(File) ->
  string(Contents, File, nil).
string(Contents, File, _Dest) ->
  Ast = 'string_to_ast!'(Contents, 1, File, []),
  Env = kapok_env:env_for_eval([{line, 1}, {file, File}]),
  {Erl, _NewEnv, _NewScope} = ast_to_abstract_format(Ast, Env),
  eval_abstract_format(Erl, Env).

%% Evaluation

eval_abstract_format(Forms, _Env) ->
  %% TODO add impl
  io:format("~p~n", [Forms]),
  Forms.

