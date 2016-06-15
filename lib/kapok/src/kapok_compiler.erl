%% Compiler for kapok
-module(kapok_compiler).
-export([file/1,
         file/2,
         string_to_ast/4,
         'string_to_ast!'/4,
         eval/2,
         eval/3,
         eval_ast/3
        ]).
-export([core/0,
         module/4]).
-import(kapok_utils, [to_binary/1]).
-include("kapok.hrl").

%% Compilation entry points.

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

%% Convertion

%% Converts a given string (char list) into AST.
string_to_ast(String, StartLine, File, Options) when is_integer(StartLine), is_binary(File) ->
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
  {EAF, TEnv} = kapok_translate:translate(EAst, EEnv),
  {EAF, TEnv}.

%% Evaluation

%% String Evaluation
eval(String, Bindings) ->
  eval(String, Bindings, []).

eval(String, Bindings, Options) when is_list(Options) ->
  eval(String, Bindings, kapok_env:env_for_eval(Options));
eval(String, Bindings, #{line := Line, file := File} = Env)
    when is_list(String), is_list(Bindings), is_integer(Line), is_binary(File) ->
  Ast = 'string_to_ast!'(String, Line, File, []),
  eval_ast(Ast, Bindings, Env).

%% AST Evaluation
eval_ast(Ast, Bindings, Options) when is_list(Options) ->
  eval_ast(Ast, Bindings, kapok_env:env_for_eval(Options));
eval_ast(Ast, Bindings, Env) ->
  Env1 = kapok_env:add_bindings(Env, Bindings),
  {Forms, TEnv1} = ast_to_abstract_format(Ast, Env1),
  eval_abstract_format(Forms, TEnv1).

%% Abstract Format Evaluation
eval_abstract_format(Forms, Env) when is_list(Forms) ->
  list:mapfoldl(fun eval_abstract_format/2, Env, Forms);
eval_abstract_format(Form, #{scope := Scope} = Env) ->
  case Form of
    {atom, _, Atom} ->
      {Atom, Env};
    _ ->
      Vars = maps:get(vars, Scope),
      {value, Value, NewBindings} = eval_erl(Form, Vars, Env),
      {Value, Env#{scope => Scope#{vars => orddict:from_list(NewBindings)}}}
  end.

eval_erl(Form, Bindings, Env) ->
  case erl_eval:check_command([Form], Bindings) of
    ok -> ok;
    {error, Desc} -> kapok_error:handle_file_error(?m(Env, file), Desc)
  end,

  %% Below must be all one line for locations to be the same when the stacktrace
  %% needs to be extended to the full stacktrace.
  try erl_eval:expr(Form, Bindings) catch Class:Exception -> erlang:raise(Class, Exception, get_stacktrace()) end.

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
module(Forms, Options, #{file := File} = _Env, Callback) ->
  Source = kapok_utils:characters_to_list(File),
  io:format("module() forms: ~p~n~n", [Forms]),
  case compile:noenv_forms([no_auto_import() | Forms], [return, {source,Source} | Options]) of
    {ok, ModuleName, Binary, Warning} ->
      format_warnings(Warning),
      {module, Module} = code:load_binary(ModuleName, binary_to_list(File), Binary),
      Callback(Module, Binary);
    {error, Errors, Warnings} ->
      format_warnings(Warnings),
      format_errors(Errors)
  end.

no_auto_import() ->
  {attribute, 0, compile, no_auto_import}.

%% CORE HANDLING

core() ->
  %% TODO add impl
  ok.

%% ERROR HANDLING
format_errors([]) ->
  exit({nocompile, "compilation failed but no error was raised"});
format_errors(Errors) ->
  lists:foreach(fun({File, Each}) ->
                    BinFile = kapok_utils:characters_to_binary(File),
                    lists:foreach(fun(Error) ->
                                      kapok_error:handle_file_error(BinFile, Error)
                                  end,
                                  Each)
                end,
                Errors).

format_warnings(Warnings) ->
  lists:foreach(fun({File, Each}) ->
                    BinFile =  kapok_utils:characters_to_binary(File),
                    lists:foreach(fun(Warning) ->
                                      kapok_error:handle_file_warning(BinFile, Warning)
                                  end,
                                  Each)
                end,
                Warnings).

