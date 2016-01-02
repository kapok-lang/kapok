%% The compilation time environment for a module
-module(ceiba_env).
-export([new/0,
         env_to_scope/1,
         env_to_scope_with_vars/2]).
-include("ceiba.hrl").

new() ->
    #{'__struct__' => 'Ceiba.Macro.Env',
      module => nil,
      file => <<"nofile">>,
      line => 1,
      function => nil,
      context => nil,
      requires => [],
      aliases => [],
      functions => [],
      macros => [],
      macro_aliases => [],
      context_modules => [],
      vars => [],
      lexical_tracker => nil}.


env_to_scope(#{module := Module,
               file := File,
               function := Function,
               context := Context}) ->
    #ceiba_scope{module = Module,
                 file = File,
                 function = Function,
                 context = Context}.

env_to_scope_with_vars(Env, Vars) ->
    (env_to_scope(Env))#ceiba_scope{vars = orddict:from_list(Vars)}.

