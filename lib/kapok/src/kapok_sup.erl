-module(kapok_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 2000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Workers = [
             ?CHILD(kapok_env, worker),
             ?CHILD(kapok_symbol_table, worker),
             ?CHILD(kapok_code, worker)
            ],
  {ok, {{one_for_one, 5, 10}, Workers}}.
