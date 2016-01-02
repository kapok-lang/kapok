%% Helper functions used to manipulate scope and its variables.
-module(ceiba_scope).
-export([mergev/2,
         mergef/2]).
-include("ceiba.hrl").

%% Scope merging

%% Receives two scopes and return a new scope based on
%% the second with their variables merged.
mergev(S1, S2) ->
    S2#ceiba_scope{
      vars = merge_vars(S1#ceiba_scope.vars, S2#ceiba_scope.vars),
      export_vars = merge_opt_vars(S1#ceiba_scope.export_vars, S2#ceiba_scope.export_vars)
     }.

%% Receives two scopes and return the first scope with
%% counters and flags from the later.
mergef(S1, S2) ->
    S1#ceiba_scope{
      super = S2#ceiba_scope.super,
      caller = S2#ceiba_scope.caller
     }.

%% Mergers.

merge_vars(V, V) -> V;
merge_vars(V1, V2) -> orddict:merge(fun var_merger/3, V1, V2).

merge_opt_vars(nil, _C2) -> nil;
merge_opt_vars(_C1, nil) -> nil;
merge_opt_vars(C, C) -> C;
merge_opt_vars(C1, C2) -> orddict:merge(fun var_merger/3, C1, C2).

var_merger(_Var, {_, V1} = K1, {_, V2}) when V1 > V2 -> K1;
var_merger(_Var, _K1, K2) -> K2.

