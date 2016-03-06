%% The compilation time environment for a module
-module(kapok_env).
-export([new/0,
         env_to_scope/1,
         env_to_scope_with_vars/2]).
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

