%% Code server for kapok compilation.
-module(kapok_code).
-behaviour(gen_server).
-export([is_loaded/1,
         load_ns/2,
         get_module/1]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-include("kapok.hrl").

%% Map namespace to Erlang module with randomly generated name.

%% Public API
is_loaded(Ns) ->
  gen_server:call(?MODULE, {is_loaded, Ns}).

load_ns(Ns, Env) ->
  gen_server:call(?MODULE, {load_ns, Ns, Env}).

get_module(Ns) ->
  gen_server:call(?MODULE, {get_module, Ns}).


%% gen_server API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, maps:new()}.

handle_call({is_loaded, Ns}, _From, NsToModules) ->
  {reply, ns_is_loaded(Ns, NsToModules), NsToModules};
handle_call({load_ns, Ns, Env}, _From, NsToModules) ->
  State = case get_ns(Ns, NsToModules) of
            {badkey, Ns} ->
              load_ns(Ns, Env, NsToModules);
            Ns ->
              unload_ns(Ns),
              NsToModules1 = remove_ns(Ns, NsToModules),
              load_ns(Ns, Env, NsToModules1)
          end,
  {reply, ok, State};
handle_call({get_module, Ns}, _From, NsToModules) ->
  R = case get_ns(Ns, NsToModules) of
        {badkey, Ns} -> not_exist;
        N -> {ok, N}
      end,
  {reply, R, NsToModules}.

handle_cast({_, _}, State) ->
  {noreply, State}.

handle_info({_, _}, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% helper functions.

ns_is_loaded(Ns, NsToModules) ->
  has_ns(Ns, NsToModules) orelse is_loaded_to_evm(Ns).

has_ns(Ns, NsToModules) ->
  maps:is_key(Ns, NsToModules).

is_loaded_to_evm(Module) ->
  case code:is_loaded(Module) of
    {file, _} -> true;
    false -> false
  end.

get_ns(Ns, NsToModules) ->
  case has_ns(Ns, NsToModules) of
    true ->
      maps:get(Ns, NsToModules);
    false ->
      %% This module is load to evm but not recorded in `kapok_code`.
      Ns
  end.

add_ns(Ns, Module, NsToModules) ->
  maps:put(Ns, Module, NsToModules).

remove_ns(Ns, NsToModules) ->
  maps:remove(Ns, NsToModules).

load_ns(Ns, Env, NsToModules) ->
  Next = next(Ns),
  {Forms, Env1} = kapok_symbol_table:namespace_forms(Ns, Next, Env),
  Callback = fun(_Module, _Binary) -> ok end,
  kapok_erl:module(Forms, [], Env1, Callback),
  %% update internal state
  add_ns(Ns, Next, NsToModules).

next(Ns) ->
  kapok_utils:gensym(Ns).

unload_ns(Ns) ->
  %% remove the old module
  code:delete(Ns),
  ok.
