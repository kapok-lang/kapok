%% The compilation time context for a module
-module(kapok_ctx).
-export([new_ctx/0,
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
         ctx_for_eval/1,
         ctx_for_eval/2]).
-import(kapok_env, [get_compiler_opt/1]).
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

new_ctx() ->
  #{'__struct__' => 'Kapok.Ctx',
    namespace => nil,                      %% the current namespace
    file => <<"nofile">>,                  %% the current filename
    line => 1,                             %% the current line
    def_kind => nil,                       %% the kind of def*
    def_fap => nil,                        %% the {Fun, Arity, ParameterType} of def*
    def_ast => nil,                        %% the ast of def*
    context => nil,                        %% can be pattern, let_pattern, guards, or nil
    macro_context => new_macro_context(),  %%
    requires => [],           %% a dict of modules(and aliases) required in 'name -> original'
    uses => [],               %% a dict of modules used in 'module -> use arguments'
    functions => [],          %% a dict of imported functions(and aliases) by 'module -> [fun...]'
    macros => [],             %% a dict of imported macros(aliases) by 'module -> [macro...]'
    scope => new_scope()      %% the current scope
   }.

add_require(Meta, Ctx, Require) when is_atom(Require) ->
  add_require(Meta, Ctx, Require, Require).
add_require(Meta, #{requires := Requires} = Ctx, Alias, Original)
    when is_atom(Alias), is_atom(Original) ->
  case orddict:find(Alias, Requires) of
    {ok, Original} ->
      %% Skip when the specified require is added already.
      Ctx;
    {ok, Other} ->
      kapok_error:compile_error(Meta, ?m(Ctx, file),
                                "invalid require ~p as ~p, it conflicts with the previous ~p as ~p",
                                [Alias, Original, Alias, Other]);
    error ->
      NewRequires = orddict:store(Alias, Original, Requires),
      Ctx#{requires => NewRequires}
  end.

add_use(Meta, #{uses := Uses} = Ctx, Module) ->
  Ctx#{uses => orddict:store(Module, [{meta, Meta}], Uses)}.

add_use(_Meta, #{uses := Uses} = Ctx, Module, Key, Value) ->
  case orddict:find(Module, Uses) of
    {ok, Args} ->
      NewArgs = orddict:store(Key, Value, Args),
      Ctx#{uses => orddict:store(Module, NewArgs, Uses)};
    error ->
      NewArgs = [{Key, Value}],
      Ctx#{uses => orddict:store(Module, NewArgs, Uses)}
  end.

add_function(_Meta, #{functions := Functions} = Ctx, Module, ToImports) ->
  case orddict:find(Module, Functions) of
    {ok, Imports} ->
      NewImports = ordsets:union(Imports, ordsets:from_list(ToImports)),
      Ctx#{functions => orddict:store(Module, NewImports, Functions)};
    error ->
      NewImports = ordsets:from_list(ToImports),
      Ctx#{functions => orddict:store(Module, NewImports, Functions)}
  end.

add_macro(_Meta, #{macros := Macros} = Ctx, Module, ToImports) ->
  case orddict:find(Module, Macros) of
    {ok, Imports} ->
      NewImports = ordsets:union(Imports, ordsets:from_list(ToImports)),
      Ctx#{macros => orddict:store(Module, NewImports, Macros)};
    error ->
      NewImports = ordsets:from_list(ToImports),
      Ctx#{macros => orddict:store(Module, NewImports, Macros)}
  end.

reset_macro_context(Ctx) ->
  Ctx#{macro_context => new_macro_context()}.

push_scope(#{scope := Scope} = Ctx) ->
  NewScope = (new_scope())#{parent => Scope},
  Ctx#{scope => NewScope}.

pop_scope(#{scope := Scope} = Ctx) ->
  case maps:get(parent, Scope) of
    nil -> Ctx;
    ParentScope -> Ctx#{scope => ParentScope}
  end.

keywords() ->
  ['ns', 'defn', 'defn-', 'defmacro', 'defmacro-',
   'let', 'do', 'case', 'fn', 'when', 'try'].

var_exist(Vars, Var) ->
  Var =/= '_' andalso orddict:is_key(Var, Vars).

is_valid_var_name(Var) ->
  Keywords = ordsets:from_list(keywords()),
  case ordsets:is_element(Var, Keywords) of
    true -> false;
    false -> true
  end.

add_var(Meta, Ctx, Var) ->
  add_var(Meta, Ctx, Var, Var).

add_let_var(Meta, Ctx, Var) ->
  V = atom_to_list(Var),
  Prefix = case V of
             [$_ | _T] -> "_VAR";
             _ -> "VAR"
           end,
  Name = kapok_utils:gensym_plain(io_lib:format("~s_~s_", [Prefix, V])),
  add_var(Meta, Ctx, Var, Name).

add_var(Meta, #{scope := Scope} = Ctx, Var, Name) ->
  case is_valid_var_name(Var) of
    true -> ok;
    false -> kapok_error:compile_error(Meta, ?m(Ctx, file), "invalid var name: ~s", [Var])
  end,
  Vars = maps:get(vars, Scope),
  case var_exist(Vars, Var) of
    true -> kapok_error:compile_error(Meta, ?m(Ctx, file),
                                      "redeclare symbol: ~p, vars: ~p", [Var, Vars]);
    false -> ok
  end,
  NewVars = orddict:store(Var, Name, Vars),
  Ctx1 = Ctx#{scope => Scope#{vars => NewVars}},
  {Name, Ctx1}.

add_bindings(Ctx, Bindings) ->
  lists:mapfoldl(fun({K, _V}, C) -> add_var([], C, K) end, Ctx, Bindings).

get_var(Meta, #{scope := Scope} = Ctx, Var) ->
  get_var(Meta, Ctx, Scope, Var).
get_var(_Meta, _Ctx, nil, _Var) ->
  error;
get_var(Meta, Ctx, Scope, Var) ->
  Vars = maps:get(vars, Scope),
  case orddict:find(Var, Vars) of
    {ok, _Name} = R -> R;
    error -> get_var(Meta, Ctx, maps:get(parent, Scope), Var)
  end.

%% EVAL HOOKS

setup_ctx(Ctx) ->
  case get_compiler_opt(internal) of
    true -> Ctx;
    false -> kapok_ast:add_default_uses(Ctx)
  end.

ctx_for_eval(Opts) ->
  Ctx = ctx_for_eval((new_ctx())#{requires := kapok_dispatch:default_requires()},
                     Opts),
  setup_ctx(Ctx).

ctx_for_eval(Ctx, Opts) ->
  Namespace = case lists:keyfind(namespace, 1, Opts) of
                {namespace, NamespaceOpt} when is_list(NamespaceOpt) -> NamespaceOpt;
                false -> nil
              end,

  File = case lists:keyfind(file, 1, Opts) of
           {file, FileOpt} when is_binary(FileOpt) -> FileOpt;
           false -> ?m(Ctx, file)
         end,

  Line = case lists:keyfind(line, 1, Opts) of
           {line, LineOpt} when is_integer(LineOpt) -> LineOpt;
           false -> ?m(Ctx, line)
         end,

  Requires = case lists:keyfind(requires, 1, Opts) of
               {requires, RequiresOpt} when is_list(RequiresOpt) ->
                 lists:map(fun({_Alias, _Original} = E) -> E;
                              (Require) when is_atom(Require) -> {Require, Require}
                           end,
                           RequiresOpt);
               false -> ?m(Ctx, requires)
             end,

  Functions = case lists:keyfind(functions, 1, Opts) of
                {functions, FunctionsOpt} when is_list(FunctionsOpt) -> FunctionsOpt;
                false -> ?m(Ctx, functions)
              end,

  Macros = case lists:keyfind(macros, 1, Opts) of
             {macros, MacrosOpt} when is_list(MacrosOpt) -> MacrosOpt;
             false -> ?m(Ctx, macros)
           end,

  Ctx#{
      namespace := Namespace,
      file := File,
      line := Line,
      requires := Requires,
      functions := Functions,
      macros := Macros
     }.
