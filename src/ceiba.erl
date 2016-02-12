%% Main entry point for Ceiba functions. All of those functions are
%% private to the Ceiba compiler and reserved to be used by Ceiba only.
-module(ceiba).
%%-behaviour(application).
-export([string_to_ast/4,
         'string_to_ast!'/4,
         ast_to_abstract_format/2,
         ast_to_abstract_format/3,
         env_for_eval/1,
         env_for_eval/2]).
-include("ceiba.hrl").

%% eval hooks

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
