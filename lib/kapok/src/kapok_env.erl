%% The compilation time environment for a module
-module(kapok_env).
-export([new_env/0,
         add_require/3,
         add_require/4,
         add_use/3,
         add_use/5,
         add_function/4,
         add_macro/4,
         reset_macro_context/1,
         push_scope/1,
         pop_scope/1,
         add_var/3,
         add_let_var/3,
         add_bindings/2,
         get_var/3,
         env_for_eval/1,
         env_for_eval/2]).

-include("kapok.hrl").

new_macro_context() ->
  #{'__struct__' => 'Kapok.MacroContext',
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
    context => nil,                        %% can be pattern, let_pattern, guards, or nil
    macro_context => new_macro_context(),  %%
    requires => [],                        %% a dict of modules(and aliases) required in 'name -> original'
    uses => [],                            %% a dict of modules used in 'module -> use arguments'
    functions => [],                       %% a dict of imported functions(and aliases) by 'module -> [fun...]'
    macros => [],                          %% a dict of imported macros(aliases) by 'module -> [macro...]'
    scope => new_scope()                   %% the current scope
   }.

add_require(Meta, Env, Require) when is_atom(Require) ->
  add_require(Meta, Env, Require, Require).
add_require(Meta, #{requires := Requires} = Env, Alias, Original) when is_atom(Alias), is_atom(Original) ->
  %% check for duplicate
  case orddict:is_key(Alias, Requires) of
    true -> kapok_error:compile_error(Meta, ?m(Env, file), "duplicate require: ~p", [Alias]);
    false -> ok
  end,
  NewRequires = orddict:store(Alias, Original, Requires),
  Env#{requires => NewRequires}.

add_use(Meta, #{uses := Uses} = Env, Module) ->
  Env#{uses => orddict:store(Module, [{meta, Meta}], Uses)}.

add_use(_Meta, #{uses := Uses} = Env, Module, Key, Value) ->
  case orddict:find(Module, Uses) of
    {ok, Args} ->
      NewArgs = orddict:store(Key, Value, Args),
      Env#{uses => orddict:store(Module, NewArgs, Uses)};
    error ->
      NewArgs = [{Key, Value}],
      Env#{uses => orddict:store(Module, NewArgs, Uses)}
  end.

add_function(_Meta, #{functions := Functions} = Env, Module, ToImports) ->
  case orddict:find(Module, Functions) of
    {ok, Imports} ->
      NewImports = ordsets:union(Imports, ordsets:from_list(ToImports)),
      Env#{functions => orddict:store(Module, NewImports, Functions)};
    error ->
      NewImports = ordsets:from_list(ToImports),
      Env#{functions => orddict:store(Module, NewImports, Functions)}
  end.

add_macro(_Meta, #{macros := Macros} = Env, Module, ToImports) ->
  case orddict:find(Module, Macros) of
    {ok, Imports} ->
      NewImports = ordsets:union(Imports, ordsets:from_list(ToImports)),
      Env#{macros => orddict:store(Module, NewImports, Macros)};
    error ->
      NewImports = ordsets:from_list(ToImports),
      Env#{macros => orddict:store(Module, NewImports, Macros)}
  end.

reset_macro_context(Env) ->
  Env#{macro_context => new_macro_context()}.

push_scope(#{scope := Scope} = Env) ->
  NewScope = (new_scope())#{parent => Scope},
  Env#{scope => NewScope}.

pop_scope(#{scope := Scope} = Env) ->
  case maps:get(parent, Scope) of
    nil -> Env;
    ParentScope -> Env#{scope => ParentScope}
  end.

keywords() ->
  ['ns', 'defn', 'defn-', 'defmacro', 'defmacro-',
   'let', 'do', 'case', 'fn', 'when', 'try'].

var_exist(Vars, Var) ->
  orddict:is_key(Var, Vars).

is_valid_var_name(Var) ->
  Keywords = ordsets:from_list(keywords()),
  case ordsets:is_element(Var, Keywords) of
    true -> false;
    false -> true
  end.

add_var(Meta, Env, Var) ->
  add_var(Meta, Env, Var, Var).

add_let_var(Meta, Env, Var) ->
  Prefix = io_lib:format("VAR_~s_", [atom_to_list(Var)]),
  Name = kapok_utils:gensym_with(Prefix),
  add_var(Meta, Env, Var, Name).

add_var(Meta, #{scope := Scope} = Env, Var, Name) ->
  case is_valid_var_name(Var) of
    true -> ok;
    false -> kapok_error:compile_error(Meta, ?m(Env, file), "invalid var name: ~s", [Var])
  end,
  Vars = maps:get(vars, Scope),
  case var_exist(Vars, Var) of
    true -> kapok_error:compile_error(Meta, ?m(Env, file), "redeclare symbol: ~p, vars: ~p", [Var, Vars]);
    false -> ok
  end,
  NewVars = orddict:store(Var, Name, Vars),
  Env1 = Env#{scope => Scope#{vars => NewVars}},
  {Name, Env1}.

add_bindings(Env, Bindings) ->
  lists:mapfoldl(fun({K, _V}, E) -> add_var([], E, K) end, Env, Bindings).

get_var(Meta, #{scope := Scope} = Env, Var) ->
  get_var(Meta, Env, Scope, Var).
get_var(_Meta, _Env, nil, _Var) ->
  error;
get_var(Meta, Env, Scope, Var) ->
  Vars = maps:get(vars, Scope),
  case orddict:find(Var, Vars) of
    {ok, _Name} = R -> R;
    error -> get_var(Meta, Env, maps:get(parent, Scope), Var)
  end.

%% EVAL HOOKS

env_for_eval(Opts) ->
  env_for_eval((new_env())#{requires := kapok_dispatch:default_requires(),
                            functions := kapok_dispatch:default_functions(),
                            macros := kapok_dispatch:default_macros()},
               Opts).

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
               {requires, RequiresOpt} when is_list(RequiresOpt) ->
                 lists:map(fun({_Alias, _Original} = E) -> E;
                              (Require) when is_atom(Require) -> {Require, Require}
                           end,
                           RequiresOpt);
               false -> ?m(Env, requires)
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
      functions := Functions,
      macros := Macros
     }.
