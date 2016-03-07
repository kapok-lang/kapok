%% The compilation time environment for a module
-module(kapok_env).
-export([new/0,
         env_to_scope/1,
         env_to_scope_with_vars/2,
         env_for_eval/1,
         env_for_eval/2]).
-include("kapok.hrl").

new() ->
  #{'__struct__' => 'Kapok.Macro.Env',
    module => nil,                         %% the current module
    file => <<"nofile">>,                  %% the current filename
    line => 1,                             %% the current line
    function => nil,                       %% the current function
    context => nil,                        %% can be match_vars, guards, or nil
    requires => [],                        %% a set with modules required
    aliases => [],                         %% an orddict with aliases by new -> old names
    functions => [],                       %% a list with functions imported
    macros => [],                          %% a list with macros imported
    macro_aliases => [],                   %% keep aliases defined inside a macro
    vars => [],                            %% a set of defined variables
    lexical_tracker => nil}.               %% holds the lexical tracker pid

env_to_scope(#{module := Module,
               file := File,
               function := Function,
               context := Context}) ->
  #kapok_scope{module = Module,
               file = File,
               function = Function,
               context = Context}.

env_to_scope_with_vars(Env, Vars) ->
  (env_to_scope(Env))#kapok_scope{vars = orddict:from_list(Vars)}.


%% EVAL HOOKS

env_for_eval(Opts) ->
  env_for_eval((new())#{
                   requires := kapok_dispatch:default_requires(),
                   functions := kapok_dispatch:default_functions(),
                   macros := kapok_dispatch:default_macros()},
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
