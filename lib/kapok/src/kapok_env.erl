%% The compilation time environment for a module
-module(kapok_env).
-export([new_env/0,
         add_require/3,
         add_module_alias/4,
         add_function/4,
         add_functions/3,
         add_function_aliases/3,
         reset_macro_context/1,
         add_export_function/3,
         add_export_macro/3,
         get_exports/1,
         push_scope/1,
         pop_scope/1,
         add_var/3,
         env_for_eval/1,
         env_for_eval/2]).

-include("kapok.hrl").

new_macro_context() ->
  #{'__struct__' => 'Kapok.MacroContext',
    quote => false,                        %% whether in quote form
    backquote_level => 0,                  %% the level in backquote form (probably embedded)
    unquote_level => 0,                    %% the level in unquote form (probably embedded)
    form => nil                            %% the body of current macro
   }.

new_scope() ->
  #{'__struct__' => 'Kapok.Scope',
    parent => nil,                         %% parent scope
    vars => []                             %% a set of defined variables
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
    export_functions => [],                %% a set of function to export
    export_macros => [],                   %% a set of macros to export
    scope => new_scope()                   %% the current scope
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
  Duplicates = orddict:filter(fun (K, _V) -> orddict:is_key(K, Functions) end, ToAdd),
  case orddict:size(Duplicates) of
    0 -> ok;
    _ -> kapok_error:compile_error(Meta, ?m(Env, file), "duplicate functions: ~p", [Duplicates])
  end,
  Env#{functions => orddict:merge(fun (_K, _V1, V2) -> V2 end, Functions, ToAdd)}.

add_function_aliases(Meta, #{functions := Functions, function_aliases := Aliases} = Env, ToAdd) when is_list(ToAdd) ->
  %% check for original names
  Absent = orddict:filter(fun (_K, V) -> orddict:is_key(V, Functions) == false end, ToAdd),
  case orddict:size(Absent) of
    0 -> ok;
    _ -> kapok_error:compile_error(Meta, ?m(Env, file), "couldnot rename unimported function: ~p", [Absent])
  end,
  %% check for duplicate
  Duplicates = orddict:filter(fun (K, _V) -> orddict:is_key(K, Aliases) end, ToAdd),
  case orddict:size(Duplicates) of
    0 -> ok;
    _ -> kapok_error:compile_error(Meta, ?m(Env, file), "duplicate function aliases: ~p", [Duplicates])
  end,
  Env#{function_aliases => orddict:merge(fun (_K, _V1, V2) -> V2 end, Aliases, ToAdd)}.

reset_macro_context(Env) ->
  Env#{macro_context => new_macro_context()}.

add_export_function(Meta, #{export_functions := ExportFunctions, export_macros := ExportMacros} = Env, Export) ->
  %% check for duplicate function
  case ordsets:is_element(Export, ExportFunctions) of
    true -> kapok_error:compile_error(Meta, ?m(Env, file), "duplicate export function: ~p", [Export]);
    false -> ok
  end,
  %% check for duplicate macro
  case ordsets:is_element(Export, ExportMacros) of
    true -> kapok_error:compile_error(Meta, ?m(Env, file), "duplicate macro for export function: ~p", [Export]);
    false -> ok
  end,
  NewExportFunctions = ordsets:add_element(Export, ExportFunctions),
  Env#{export_functions => NewExportFunctions}.

add_export_macro(Meta, #{export_functions := ExportFunctions, export_macros := ExportMacros} = Env, Export) ->
  %% check for duplicate macro
  io:format("add export macro: ~p~n", [Export]),
  case ordsets:is_element(Export, ExportMacros) of
    true -> kapok_error:compile_error(Meta, ?m(Env, file), "duplicate export macro: ~p", [Export]);
    false -> ok
  end,
  %% check for duplicate function
  case ordsets:is_element(Export, ExportFunctions) of
    true -> kapok_error:compile_error(Meta, ?m(Env, file), "duplicate function for export macro: ~p", [Export]);
    false -> ok
  end,
  NewExportMacros = ordsets:add_element(Export, ExportMacros),
  Env#{export_macros => NewExportMacros}.

get_exports(#{export_functions := ExportFunctions, export_macros := ExportMacros}) ->
  io:format("export functions and macros: ~p~n~p~n", [ExportFunctions, ExportMacros]),
  ordsets:to_list(ordsets:union(ExportFunctions, ExportMacros)).

push_scope(#{scope := Scope} = Env) ->
  NewScope = (new_scope())#{parent => Scope, vars => maps:get(vars, Scope)},
  Env#{scope => NewScope}.

pop_scope(#{scope := Scope} = Env) ->
  case maps:get(parent, Scope) of
    nil -> Env;
    ParentScope -> Env#{scope => ParentScope}
  end.

add_var(Meta, #{scope := Scope} = Env, Var) ->
  Vars = maps:get(vars, Scope),
  case orddict:is_key(Var, Vars) of
    true -> kapok_error:compile_error(Meta, ?m(Env, file), "duplicate var: ~p", [Var]);
    false -> ok
  end,
  NewVars = orddict:store(Var, {var,0,Var}, Vars),
  Env#{Scope => Scope#{vars => NewVars}}.


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
