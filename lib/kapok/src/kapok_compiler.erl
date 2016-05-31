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
  {EAst, EEnv} = kapok_expand:expand_all(Ast, Env),
  {Erl, TEnv} = kapok_translate:translate(EAst, EEnv),
  {Erl, TEnv}.

%% Compilation entry points.

ast(Ast, File) when is_binary(File) ->
  Env = kapok_env:env_for_eval([{line, 1}, {file, File}]),
  ast(Ast, Env);
ast(Ast, Env) ->
  {Forms, TEnv} = ast_to_abstract_format(Ast, Env),
  eval_abstract_format(Forms, TEnv).

string(Contents, File) when is_list(Contents), is_binary(File) ->
  Ast = 'string_to_ast!'(Contents, 1, File, []),
  ast(Ast, File).

file(Relative) when is_binary(Relative) ->
  file(Relative, nil).
file(File, Outdir) ->
  {ok, Bin} = file:read_file(File),
  Contents = kapok_utils:characters_to_list(Bin),
  Ast = 'string_to_ast!'(Contents, 1, File, []),
  Env = kapok_env:env_for_eval([{line, 1}, {file, File}]),
  AfterModuleCompiled = fun(Module, Binary) ->
                            %% write compiled binary to dest file
                            case Outdir of
                              nil ->
                                try
                                  Module:main()
                                catch
                                  error:undef -> ok
                                end;
                              _ ->
                                Outfile = filename:join(Outdir, atom_to_list(Module) ++ ?BEAM_FILE_SUFFIX),
                                ok = file:write_file(Outfile, Binary)
                            end
                        end,
  kapok_namespace:compile(Ast, Env, AfterModuleCompiled).

%%
core() ->
  %% TODO add impl
  ok.

%% Evaluation

eval_abstract_format(Form, #{scope := Scope} = Env) ->
  case Form of
    {atom, _, Atom} ->
      {Atom, Env};
    _ ->
      Vars = maps:get(vars, Scope),
      {value, Value, NewBindings} = erl_eval(Form, Vars, Env),
      {Value, Env#{scope => Scope#{vars => orddict:from_list(NewBindings)}}}
  end.

erl_eval(Erl, Binding, Env) ->
  case erl_eval:check_command([Erl], Binding) of
    ok -> ok;
    {error, Desc} -> kapok_error:handle_file_error(?m(Env, file), Desc)
  end,

  %% Below must be all one line for locations to be the same when the stacktrace
  %% needs to be extended to the full stacktrace.
  try erl_eval:expr(Erl, Binding) catch Class:Exception -> erlang:raise(Class, Exception, get_stacktrace()) end.

get_stacktrace() ->
  Stacktrace = erlang:get_stacktrace(),
  %% eval_eval and eval_bits can call :erlang.raise/3 without the full
  %% stacktrace. When this occurs re-add the current stacktrace so that no
  %% stack information is lost.
  try
    throw(stack)
  catch
    throw:stack ->
      % Ignore stack item for current function.
      [_ | CurrentStack] = erlang:get_stacktrace(),
      get_stacktrace(Stacktrace, CurrentStack)
  end.

%% The stacktrace did not include the current stack, re-add it.
get_stacktrace([], CurrentStack) ->
  CurrentStack;
%% The stacktrace includes the current stack.
get_stacktrace(CurrentStack, CurrentStack) ->
  CurrentStack;
get_stacktrace([StackItem | Stacktrace], CurrentStack) ->
  [StackItem | get_stacktrace(Stacktrace, CurrentStack)].

%% INTERNAL API

%% Compile the module by forms based on the scope information
%% executes the callback in case of success. This automatically
%% handles errors and warnings. Used by this module.
module(Forms, _Opts, #{file := File} = _Env, Callback) ->
  io:format("module() forms: ~p~n~n", [Forms]),
  case compile:forms(Forms) of
    {ok, ModuleName, Binary} ->
      {module, Module} = code:load_binary(ModuleName, binary_to_list(File), Binary),
      Callback(Module, Binary);
    {error, Errors, Warnings} ->
      io:format("~p~n", [Errors]),
      io:format("~p~n", [Warnings])
  end.

