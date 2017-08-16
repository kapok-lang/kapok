%% Compiler for kapok
-module(kapok_compiler).
-export([file/1,
         file/2,
         string_to_ast/4,
         'string_to_ast!'/4,
         eval/2,
         eval/3,
         eval_ast/2,
         eval_ast/3]).
-export([core/0]).
-import(kapok_utils, [to_binary/1]).
-include("kapok.hrl").

%% Public API

%% Compilation entry points.

file(Relative) when is_binary(Relative) ->
  file(Relative, nil).
file(File, Outdir) ->
  {ok, Bin} = file:read_file(File),
  Contents = kapok_utils:characters_to_list(Bin),
  Ast = 'string_to_ast!'(Contents, 1, File, []),
  Ctx = kapok_ctx:ctx_for_eval([{line, 1}, {file, File}]),
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
  kapok_ast:compile(Ast, Ctx, AfterModuleCompiled).

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
ast_to_abstract_format(Ast, Ctx) ->
  kapok_trans:translate(Ast, Ctx).

%% Evaluation

%% String Evaluation
eval(String, Bindings) ->
  eval(String, Bindings, []).

eval(String, Bindings, Options) when is_list(Options) ->
  eval(String, Bindings, kapok_ctx:ctx_for_eval(Options));
eval(String, Bindings, #{line := Line, file := File} = Ctx)
    when is_list(String), is_list(Bindings), is_integer(Line), is_binary(File) ->
  Ast = 'string_to_ast!'(String, Line, File, []),
  eval_ast(Ast, Bindings, Ctx).

%% AST Evaluation
eval_ast(Ast, Bindings, Options) when is_list(Options) ->
  eval_ast(Ast, Bindings, kapok_ctx:ctx_for_eval(Options));
eval_ast(Ast, Bindings, Ctx) ->
  {_, Ctx1} = kapok_ctx:add_bindings(Ctx, Bindings),
  eval_ast(Ast, Ctx1).
eval_ast(Ast, Ctx) ->
  {Forms, TCtx1} = ast_to_abstract_format(Ast, Ctx),
  kapok_erl:eval_abstract_format(Forms, TCtx1).

%% CORE HANDLING

core() ->
  {ok, _} = application:ensure_all_started(kapok),
  Options = orddict:from_list([{docs, false}, {internal, true}]),
  kapok_env:update_in(compiler_options, Options),
  [load_core_libs(File) || File <- core_libs()].

load_core_libs(File) ->
  Dir = "lib/kapok/lib",
  Dest = <<"lib/kapok/ebin">>,
  F = list_to_binary(filename:join(Dir, binary_to_list(File))),
  try
    _Ctx = file(F, Dest)
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
