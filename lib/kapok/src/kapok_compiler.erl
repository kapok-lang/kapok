%% Compiler for kapok
-module(kapok_compiler).
-export([get_opt/1,
         file/1,
         file/2,
         string_to_ast/4,
         'string_to_ast!'/4,
         eval/2,
         eval/3,
         eval_ast/2,
         eval_ast/3]).
-export([core/0,
         module/4]).
-import(kapok_utils, [to_binary/1]).
-include("kapok.hrl").

%% Public API

get_opt(Key) ->
  Options = kapok_config:get(compiler_options),
  case lists:keyfind(Key, 1, Options) of
    false -> false;
    {Key, Value} -> Value
  end.

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
                                binary_to_path({Module, Binary}, Outdir)
                            end
                        end,
  kapok_ast:compile(Ast, Env, AfterModuleCompiled).

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
  kapok_translate:translate(Ast, Env).

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
  {_, Env1} = kapok_env:add_bindings(Env, Bindings),
  eval_ast(Ast, Env1).
eval_ast(Ast, Env) ->
  {Forms, TEnv1} = ast_to_abstract_format(Ast, Env),
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
  try
    erl_eval:expr(Form, Bindings)
  catch
    Class:Exception -> erlang:raise(Class, Exception, get_stacktrace())
  end.

get_stacktrace() ->
  Stacktrace = erlang:get_stacktrace(),
  %% eval_eval and eval_bits can call :erlang.raise/3 without the full
  %% stacktrace. When this occurs re-add the current stacktrace so that no
  %% stack information is lost.
  try
    throw(stack)
  catch
    throw:stack ->
      %% Ignore stack item for current function.
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
%% handles errors and warnings. Used by this module and kapok_ast.
module(Forms, Options, Env, Callback) ->
  Final = case (get_opt(debug_info) == true) orelse
            lists:member(debug_info, Options) of
            true -> [debug_info] ++ options();
            false -> options()
          end,
  compile_module(Forms, Final, Env, Callback).

compile_module(Forms, Options, #{file := File} = _Env, Callback) ->
  Source = kapok_utils:characters_to_list(File),
  case get_opt(debug) of
    true -> io:format("--- compile_module ---~n~p~n------~n", [Forms]);
    false -> ok
  end,
  case compile:noenv_forms([no_auto_import() | Forms], [return, {source, Source} | Options]) of
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

options() ->
  case kapok_config:get(erl_compiler_options) of
    nil ->
      kapok_config:update(erl_compiler_options, fun options/1);
    Options ->
      Options
  end.

options(nil) ->
  Key = "ERL_COMPILER_OPTIONS",
  case os:getenv(Key) of
    false ->
      [];
    Str when is_list(Str) ->
      case erl_scan:string(Str) of
        {ok, Tokens, _} ->
          case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
            {ok, List} when is_list(List) -> List;
            {ok, Term} -> [Term];
            {error, _Reason} ->
              io:format("Ignoring bad term in ~ts~n", [Key]),
              []
          end;
        {error, _, _} ->
          io:format("Ignoring bad term in ~ts~n", [Key]),
          []
      end
  end;
options(Options) ->
  Options.

%% CORE HANDLING

core() ->
  {ok, _} = application:ensure_all_started(kapok),
  Options = orddict:from_list([{docs, false}, {internal, true}]),
  kapok_config:update_in(compiler_options, Options),
  [load_core_libs(File) || File <- core_libs()].

load_core_libs(File) ->
  Dir = "lib/kapok/lib",
  Dest = <<"lib/kapok/ebin">>,
  F = list_to_binary(filename:join(Dir, binary_to_list(File))),
  try
    _Env = file(F, Dest)
  catch
    Kind:Reason ->
      io:format("~p: ~p~nstacktrace: ~p~n", [Kind, Reason, erlang:get_stacktrace()]),
      erlang:halt(1)
  end.

core_libs() ->
  [<<"core.kpk">>].

binary_to_path({ModuleName, Binary}, Outdir) ->
  Path = filename:join(Outdir, atom_to_list(ModuleName) ++ ?BEAM_FILE_SUFFIX),
  ok = file:write_file(Path, Binary),
  Path.

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
