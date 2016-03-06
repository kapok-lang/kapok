%%
-module(kapok_compiler).
-export([get_opt/1,
         string/2,
         quoted/2,
         file/1,
         file/2]).
-include("kapok.hrl").

%% Pubilc API

get_opt(Key) ->
  Dict = kapok_config:get(compiler_options),
  case lists:keyfind(Key, 1, Dict) of
    false -> false;
    {Key, Value} -> Value
  end.

%% Compilation entry points.

string(Contents, File) when is_list(Contents), is_binary(File) ->
  string(Contents, File, nil).
string(Contents, File, Dest) ->
  Ast = kapok:'string_to_ast!'(Contents, 1, File, []),
  quoted(Ast, File, Dest).

quoted(Forms, File) when is_binary(File) ->
  quoted(Forms, File, nil).
quoted(Forms, File, _Dest) ->
  Env = kapok:env_for_eval([{line, 1}, {file, File}]),
  %% TODO
  eval_forms(Forms, [], Env).

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

%% Evaluation

eval_forms(Forms, Vars, Env) ->
  case (?m(Env, module) == nil) andalso allows_fast_compilation(Forms) of
    true -> eval_compilation(Forms, Vars, Env);
    false -> code_loading_compilation(Forms, Vars, Env)
  end.

eval_compilation(Forms, Vars, Env) ->
  Binding = [{Key, Value} || {_Name, _Kind, Key, Value} <- Vars],
  {Result, _Binding, NewEnv, _Scope} = kapok:eval_forms(Forms, Binding, Env),
  {Result, NewEnv}.

code_loading_compilation(Forms, Vars, #{line := _Line} = Env) ->
  Dict = [{{Name, Kind}, {Value, 0}} || {Name, Kind, Value, _} <- Vars],
  Scope = kapok_env:env_to_scope_with_vars(Env, Dict),
  {Expr, _NewEnv, _NewScope} = kapok:ast_to_abstract_format(Forms, Env, Scope),
  %% TODO
  io:format("~p~n", [Expr]),
  Expr.

options() ->
  case kapok_config:get(erl_compiler_options) of
    nil ->
      kapok_config:update(erl_compiler_options, fun options/1);
    Opts ->
      Opts
  end.

options(nil) ->
  Key = "ERL_COMPILER_OPTIONS",
  case os:getenv(Key) of
    false -> [];
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
        {error, {_, _, Reason}, _} ->
          io:format("Ignoring bad term in ~ts~n", [Key]),
          []
      end
  end;
options(Opts) ->
  Opts.

dispatch_loaded(Module, Fun, Args, Purgeable, I, E) ->
  Res = Module:Fun(Args),
  code:delete(Module),
  if Purgeable ->
      code:purge(Module),
      return_module_name(I);
     true ->
      ok
  end,
  {Res, E}.

code_fun(nil) -> '__FILE__';
code_fun(_) -> '__MODULE__'.

code_mod(Fun, Expr, Line, File, Module, Vars) when is_binary(File), is_integer(Line) ->
  Tuple = {tuple, Line, [{var, Line, K} || {_, _, K, _} <- Vars]},
  Relative = elixir_utils:relative_to_cwd(File),
  [
   {attribute, Line, file, {kapok_utils:characters_to_list(Relative), 1}},
   {attribute, Line, module, Module},
   {attribute, Line, export, [{Fun, 1}, {'__RELATIVE__', 0}]},
   {function, Line, Fun, 1,
    [{clause, Line, [Tuple], [], [Expr]}]},
   {function, Line, '__RELATIVE__', 0,
    [{clause, Line, [], [], [kapok:to_abstract_format(Relative)]}]}
  ].

retrieve_module_name() ->
  kapok_code_server:call(retrieve_module_name).

return_module_name(I) ->
  kapok_code_server:cast({return_module_name, I}).

allows_fast_compilation({'__block__', _, Exprs}) ->
  lists:all(fun allows_fast_compilation/1, Exprs);
allows_fast_compilation({defmodule, _, _}) -> true;
allows_fast_compilation(_) -> false.
