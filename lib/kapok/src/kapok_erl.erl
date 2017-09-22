%% Module for handling Erlang Abstract Code and dealing with Erlang VM.
-module(kapok_erl).
-export([eval_abstract_format/2,
         module/4]).
-import(kapok_env, [get_compiler_opt/1]).
-include("kapok.hrl").


%% Abstract Format Evaluation
eval_abstract_format(Forms, Ctx) when is_list(Forms) ->
  lists:mapfoldl(fun eval_abstract_format/2, Ctx, Forms);
eval_abstract_format(Form, #{scope := Scope} = Ctx) ->
  case Form of
    {atom, _, Atom} ->
      {Atom, Ctx};
    _ ->
      Vars = maps:get(vars, Scope),
      {value, Value, NewBindings} = eval_erl(Form, Vars, Ctx),
      {Value, Ctx#{scope => Scope#{vars => orddict:from_list(NewBindings)}}}
  end.

eval_erl(Form, Bindings, Ctx) ->
  case erl_eval:check_command([Form], Bindings) of
    ok -> ok;
    {error, Desc} -> kapok_error:handle_file_error(?m(Ctx, file), Desc)
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


%% Compile the module by forms based on the scope information
%% executes the callback in case of success. This automatically
%% handles errors and warnings. Used by this module and kapok_ast.
module(Forms, Options, Ctx, Callback) ->
  Final = case (get_compiler_opt(debug_info) == true) orelse
            lists:member(debug_info, Options) of
            true -> [debug_info] ++ options();
            false -> options()
          end,
  compile_module(Forms, Final, Ctx, Callback).

compile_module(Forms, Options, #{file := File} = _Ctx, Callback) ->
  Source = kapok_utils:characters_to_list(File),
  case get_compiler_opt(debug) of
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
  case kapok_env:get(erl_compiler_options) of
    nil ->
      kapok_env:update(erl_compiler_options, fun options/1);
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
