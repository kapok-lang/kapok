%% Compiler for kapok
-module(kapok_compiler).
-export([string/2,
         ast/2,
         file/1,
         file/2]).
-export([core/0,
         module/4,
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
  {Expanded, EEnv} = kapok_expand:expand_all(Ast, Env),
  {Erl, TEnv} = kapok_translate:translate(Expanded, EEnv),
  io:format("~n after translate: ~p~n", [Erl]),
  {Erl, TEnv}.

%% Compilation entry points.

string(Contents, File) when is_list(Contents), is_binary(File) ->
  string(Contents, File, nil).
string(Contents, File, Dest) ->
  Ast = 'string_to_ast!'(Contents, 1, File, []),
  ast(Ast, File, Dest).

ast(Ast, File) when is_binary(File) ->
  ast(Ast, File, nil).
ast(Ast, File, _Dest) ->
  Env = kapok_env:env_for_eval([{line, 1}, {file, File}]),
  {Forms, TEnv} = ast_to_abstract_format(Ast, Env),
  eval_abstract_format(Forms, TEnv).

file(Relative) when is_binary(Relative) ->
  file(Relative, nil).
file(File, Dest) ->
  %% parse arguments
  case init:get_argument(run_mode) of
    {ok, [Mode]} ->
      Opts = [{run_mode, Mode}];
    _ ->
      Opts = []
  end,
  io:format("Options: ~p~n", [Opts]),
  %%
  {ok, Bin} = file:read_file(File),
  Contents = kapok_utils:characters_to_list(Bin),
  Ast = 'string_to_ast!'(Contents, 1, File, Opts),
  Env = kapok_env:env_for_eval([{line, 1}, {file, File} | Opts]),
  {Forms, TEnv} = ast_to_abstract_format(Ast, Env),
  module(Forms, [], TEnv, fun (Module, Binary) ->
                              %% write compiled binary to dest file
                              case Dest of
                                nil -> ok;
                                _ ->
                                  ok = file:write_file(Dest, Binary)
                              end,
                              %% call the main() on script mode
                              case lists:keyfind(run_mode, 1, Opts) of
                                {run_mode, ["script"]} ->
                                  try
                                    Module:main()
                                  catch
                                    error:undef -> ok
                                  end;
                                _ -> ok
                              end
                          end).

%%
core() ->
  %% TODO add impl
  ok.

%% Evaluation

eval_abstract_format(Forms, #{vars := Vars} = Env) ->
  %% TODO add impl
  %% Erl is expression?
  {value, Value, NewBindings} = erl_eval:exprs(Forms, Vars),
  {Value, Env#{vars => NewBindings}}.

module(Forms, _Opts, #{file := File} = _Env, Callback) ->
  case compile:forms(Forms) of
    {ok, ModuleName, Binary} ->
      {module, Module} = code:load_binary(ModuleName, binary_to_list(File), Binary),
      io:format("done compiling ~p~n", [ModuleName]),
      Callback(Module, Binary);
    {error, Errors, Warnings} ->
      io:format("~p~n", [Errors]),
      io:format("~p~n", [Warnings])
  end.

