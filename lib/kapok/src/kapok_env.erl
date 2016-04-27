%% The compilation time environment for a module
-module(kapok_env).
-export([new_env/0,
         env_for_eval/1,
         env_for_eval/2]).

-include("kapok.hrl").

new_macro_context() ->
  #{'__struct__' => 'Kapok.MacroContext',
    backquote_level => 0,                  %% the level in backquote form (probably embedded)
    unquote_level => 0,                    %% the level in unquote form (probably embedded)
    form => nil                            %% the body of current macro
   }.

new_env() ->
  #{'__struct__' => 'Kapok.Env',
    namespace => nil,                      %% the current namespace
    file => <<"nofile">>,                  %% the current filename
    line => 1,                             %% the current line
    function => nil,                       %% the current function
    context => nil,                        %% can be match_vars, guards, or nil
    macro_context => new_macro_context(),
    requires => [],                        %% a set with modules required
    aliases => [],                         %% an orddict with aliases by new -> old names
    functions => [],                       %% a list with functions imported
    macros => [],                          %% a list with macros imported
    macro_aliases => [],                   %% keep aliases defined inside a macro
    vars => []                             %% a set of defined variables
   }.

%% EVAL HOOKS

env_for_eval(Opts) ->
  env_for_eval(new_env(), Opts).

env_for_eval(Env, Opts) ->
  Namespace = case lists:keyfind(namespace, 1, Opts) of
             {namespace, NamespaceOpt} when is_atom(NamespaceOpt) -> NamespaceOpt;
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
      namespace := Namespace,
      file := File,
      line := Line,
      requires := Requires,
      aliases := Aliases,
      functions := Functions,
      macros := Macros
     }.
