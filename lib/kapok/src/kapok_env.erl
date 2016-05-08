%% The compilation time environment for a module
-module(kapok_env).
-export([new_env/0,
         add_require/3,
         add_module_alias/4,
         add_function/4,
         add_functions/3,
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
    macro_context => new_macro_context(),  %%
    requires => [],                        %% a set of modules required
    module_aliases => [],                  %% a dict of module aliases by 'new name -> original name'
    functions => [],                       %% a dict of imported functions by 'name -> original'
    function_aliases => [],                %% a dict of function aliases by 'new name -> original'
    macros => [],                          %% a dict of imported macros by 'name -> original'
    macro_aliases => [],                   %% a dict of macro aliases by 'new name -> original'
    vars => []                             %% a set of defined variables
   }.

add_require(Meta, #{requires := Requires} = Env, Require) ->
  %% check for duplicate
  case ordsets:is_element(Require, Requires) of
    true -> kapok_error:compile_error(Meta, ?m(Env, file), "duplicate require: ~p", [Require]);
    false -> ok
  end,
  NewRequires = ordsets:add_element(Require, Requires),
  Env#{requires => NewRequires}.

add_module_alias(Meta, #{module_aliases := Aliases} = Env, Alias, Original) ->
  %% check for duplicate
  case orddict:is_key(Alias, Aliases) of
    true -> kapok_error:compile_error(Meta, ?m(Env, file), "duplicate alias: ~p", [Alias]);
    false -> ok
  end,
  NewAliases = orddict:store(Alias, Original, Aliases),
  Env#{module_aliases => NewAliases}.

add_function(Meta, #{functions := Functions} = Env, Function, Original) ->
  %% chekt for duplicate
  case orddict:is_key(Function, Functions) of
    true -> kapok_error:compile_error(Meta, ?m(Env, file), "duplicate function: ~p", [Function]);
    false -> ok
  end,
  NewFunctions = orddict:store(Function, Original, Functions),
  Env#{functions => NewFunctions}.

add_functions(Meta, #{functions := Functions} = Env, ToAdd) when is_list(ToAdd) ->
  %% check for duplicate
  Duplicates = orddict:filter(fun(K, _) -> orddict:is_key(K, Functions) end, ToAdd),
  case orddict:size(Duplicates) of
    0 -> ok;
    _ -> kapok_error:compile_error(Meta, ?m(Env, file), "duplicate functions: ~p", [Duplicates])
  end,
  Env#{functions => orddict:merge(fun(_K, _V1, V2) -> V2 end, Functions, ToAdd)}.

%% EVAL HOOKS

env_for_eval(Opts) ->
  env_for_eval(new_env(), Opts).

env_for_eval(Env, Opts) ->
  Namespace = case lists:keyfind(namespace, 1, Opts) of
             {namespace, NamespaceOpt} when is_list(NamespaceOpt) -> NamespaceOpt;
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

  ModuleAliases = case lists:keyfind(module_aliases, 1, Opts) of
              {module_aliases, AliasesOpt} when is_list(AliasesOpt) -> AliasesOpt;
              false -> ?m(Env, module_aliases)
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
      module_aliases := ModuleAliases,
      functions := Functions,
      macros := Macros
     }.
