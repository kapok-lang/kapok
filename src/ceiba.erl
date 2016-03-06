%% Main entry point for Ceiba functions. All of those functions are
%% private to the Ceiba compiler and reserved to be used by Ceiba only.
-module(ceiba).
-behaviour(application).
-export([start_cli/0,
         string_to_ast/4,
         'string_to_ast!'/4,
         ast_to_abstract_format/2,
         ast_to_abstract_format/3,
         to_abstract_format/1,
         abstract_format_remote_call/4,
         env_for_eval/1,
         env_for_eval/2]).
-include("ceiba.hrl").

%% OTP Application API

-export([start/2, stop/1, config_change/3]).

start(_Type, _Args) ->
  %% In case there is a shell, we can't really change its encoding,
  %% so we just set binary to true. Otherwise we must set the encoding
  %% as the user with no shel has encoding to latin1.
  Opts =
    case init:get_argument(noshell) of
      {ok, _} -> [binary, {encoding, utf8}];
      error -> [binary]
    end,
  ok = io:setopts(standard_io, Opts),

  %% TODO: Remove this once we support only OTP >18
  ok = case io:setopts(standard_error, [{encoding, utf8}]) of
         ok -> ok;
         {error, _} -> io:setopts(standard_error, [{unicode, true}]) %% OTP 17.3 and earlier
       end,

  case file:native_name_encoding() of
    latin1 ->
      io:format(standard_error,
                "warning: the VM is running with native name encoding of latin1, which may cause "
                "Ceiba to malfunction as it expects utf8. Please ensure your locale is set to "
                "UTF-8~n", []);
    _ ->
      ok
  end,

  URIs = [{<<"ftp">>, 21},
          {<<"sftp">>, 22},
          {<<"tftp">>, 69},
          {<<"http">>, 80},
          {<<"https">>, 443},
          {<<"ldap">>, 389}],
  URIConfig = [{{uri, Scheme}, Port} || {Scheme, Port} <- URIs],
  CompilerOpts = [{docs, true}, {debug_info, true}, {warnings_as_errors, false}],
  Config = [{at_exit, []},
            {compiler_options, orddict:from_list(CompilerOpts)}
            | URIConfig],
  Tab = ceiba_config:new(Config),
  case ceiba_sup:start_link() of
    {ok, Sup} ->
      {ok, Sup, Tab};
    {error, _Reason} = Error ->
      ceiba_config:delete(Tab),
      Error
  end.

stop(Tab) ->
  ceiba_config:delete(Tab).

config_change(_Changed, _New, _Remove) ->
  ok.

%% Boot and process given options. Invoked by Ceiba's script.

start_cli() ->
  {ok, _} = application:ensure_all_started(?MODULE),

  %% We start the Logger so tools which depend on Ceiba always have the Logger directly accessible.
  %% However Logger is not dependency of the Ceiba application, which means releases that want to
  %% use Logger must always list it as part of its applications.
  %% TODO add standard library logger
  %% _ = case code:ensure_loaded('Ceiba.Logger') of
  %%       {module, _} -> application:start(logger);
  %%       {error, _} -> ok
  %%     end,

  %% TODO move ceiba_cli:main() to Ceiba.Core.CLI:main()
  ceiba_cli:main(init:get_plain_arguments()).

%% EVAL HOOKS

env_for_eval(Opts) ->
  env_for_eval((ceiba_env:new())#{
                   requires := ceiba_dispatch:default_requires(),
                   functions := ceiba_dispatch:default_functions(),
                   macros := ceiba_dispatch:default_macros()},
               Opts).

env_for_eval(Env, Opts) ->
  Module = case lists:keyfind(module, 1, Opts) of
             {module, ModuleOpt} when is_atom(ModuleOpt) -> ModuleOpt;
             false -> nil
           end,

  File = case lists:keyfind(file, 1, Opts) of
           {file, FileOpt} when is_binary(FileOpt) -> FileOpt;
           false -> ?m(Env, file)
         end,

  Line = case lists:keyfind(line, 1, Opts) of
           {line, LineOpt} when is_integer(LineOpt) -> LineOpt;
           false -> ?m(Env, line)
         end,

  Requires = case lists:keyfind(requires, 1, Opts) of
               {requires, RequiresOpt} when is_list(RequiresOpt) -> RequiresOpt;
               false -> ?m(Env, requires)
             end,

  Aliases = case lists:keyfind(aliases, 1, Opts) of
              {aliases, AliasesOpt} when is_list(AliasesOpt) -> AliasesOpt;
              false -> ?m(Env, aliases)
            end,

  Functions = case lists:keyfind(functions, 1, Opts) of
                {functions, FunctionsOpt} when is_list(FunctionsOpt) -> FunctionsOpt;
                false -> ?m(Env, functions)
              end,

  Macros = case lists:keyfind(macros, 1, Opts) of
             {macros, MacrosOpt} when is_list(MacrosOpt) -> MacrosOpt;
             false -> ?m(Env, macros)
           end,

  Env#{
      module := Module,
      file := File,
      line := Line,
      requires := Requires,
      aliases := Aliases,
      functions := Functions,
      macros := Macros
     }.



%% Converts AST to erlang abstract format

ast_to_abstract_format(Ast, Env) ->
  ast_to_abstract_format(Ast, Env, ceiba_env:env_to_scope(Env)).

ast_to_abstract_format(Ast, Env, Scope) ->
  {Expanded, NewEnv} = ceiba_expand:expand(Ast, Env),
  {Erl, NewScope} = ceiba_translator:translate(Expanded, Scope),
  {Erl, NewEnv, NewScope}.

%% Converts specified code to erlang abstract format

to_abstract_format(Tree) when is_tuple(Tree) ->
  {tuple, 0, [to_abstract_format(X) || X <- tuple_to_list(Tree)]};
to_abstract_format([]) ->
  {nil, 0};
to_abstract_format(<<>>) ->
  {bin, 0, []};
to_abstract_format(Tree) when is_list(Tree) ->
  to_abstract_format_cons_1(Tree, []);
to_abstract_format(Tree) when is_atom(Tree) ->
  {atom, 0, Tree};
to_abstract_format(Tree) when is_integer(Tree) ->
  {integer, 0, Tree};
to_abstract_format(Tree) when is_float(Tree) ->
  {float, 0, Tree};
to_abstract_format(Tree) when is_binary(Tree) ->
  %% Note that our binaries are utf-8 encoded and we are converting
  %% to a list using binary_to_list. The reason for this is that Erlang
  %% considers a string in a binary to be encoded in latin1, so the bytes
  %% are not changed in any fashion.
  {bin, 0, [{bin_element, 0, {string, 0, binary_to_list(Tree)}, default, default}]};
to_abstract_format(Function) when is_function(Function) ->
  case (erlang:fun_info(Function, type) == {type, external}) andalso
       (erlang:fun_info(Function, env) == {env, []}) of
    true ->
      {module, Module} = erlang:fun_info(Function, module),
      {name, Name}     = erlang:fun_info(Function, name),
      {arity, Arity}   = erlang:fun_info(Function, arity),

      {'fun', 0, {function,
        {atom, 0, Module},
        {atom, 0, Name},
        {integer, 0, Arity}}};
    false ->
      error(badarg)
  end;

to_abstract_format(Pid) when is_pid(Pid) ->
  abstract_format_remote_call(0, erlang, binary_to_term, [to_abstract_format(term_to_binary(Pid))]);
to_abstract_format(_Other) ->
  error(badarg).

to_abstract_format_cons_1([H|T], Acc) ->
  to_abstract_format_cons_1(T, [H|Acc]);
to_abstract_format_cons_1(Other, Acc) ->
  to_abstract_format_cons_2(Acc, to_abstract_format(Other)).

to_abstract_format_cons_2([H|T], Acc) ->
  to_abstract_format_cons_2(T, {cons, 0, to_abstract_format(H), Acc});
to_abstract_format_cons_2([], Acc) ->
  Acc.

abstract_format_remote_call(Line, Module, Function, Args) ->
  {call, Line, {remote, Line, {atom, Line, Module}, {atom, Line, Function}}, Args}.

%% Converts a given string (char list) into AST.

string_to_ast(String, StartLine, File, Options)
    when is_integer(StartLine), is_binary(File) ->
  case ceiba_scanner:scan(String, StartLine, [{file, File}|Options]) of
    {ok, Tokens, _EndLocation} ->
      try ceiba_parser:parse(Tokens) of
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
      Line = ceiba_scanner:location_line(Location),
      ceiba_error:parse_error(Line, File, Module, ErrorDesc)
  end.

to_binary(List) when is_list(List) -> unicode:characters_to_binary(List);
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8).
