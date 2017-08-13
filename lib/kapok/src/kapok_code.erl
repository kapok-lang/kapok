%% Code server for kapok compilation.
-module(kapok_code).
-behaviour(gen_server).
-export([is_loaded/2,
         load/3,
         call/6]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%-import(kapok_code, [load_binary]).

%% Public API
is_loaded(Module, Env) ->
  gen_server:call(?MODULE, {is_loaded, Module, Env}).

load(Module, Forms, Env) ->
  gen_server:call(?MODULE, {load, Module, Forms, Env}).

call(Meta, Module, Fun, Arity, Args, Env) ->
  get_server:call(?MODULE, {call, Meta, Module, Fun, Arity, Args, Env}).


%% gen_server API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, {0, maps:new()}}.

handle_call({is_loaded, Module, _Env}, _From, {_Counter, Names} = State) ->
  {reply, module_is_loaded(Module, Names), State};
handle_call({load, Module, _Forms, _Env}, _From, {_Counter, Names} = State) ->
  R = case get_name(Module, Names) of
        {badkey, Module} ->
          %% TODO load a new module
          ok;
        _RealName ->
          %% TODO delet an old module
          ok
      end,
  %% TODO add impl
  {reply, R, State};
handle_call({call, _Meta, _Module, _Fun, _Arity, _Args, _Env}, _From, State) ->
  %% TODO add impl
  {reply, ok, State}.

handle_cast({_, _}, State) ->
  {noreply, State}.

handle_info({_, _}, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% helper functions.

module_is_loaded(Module, Names) ->
  has_name(Module, Names) orelse is_loaded_to_evm(Module).

has_name(Module, Names) ->
  maps:is_key(Module, Names).

is_loaded_to_evm(Module) ->
  case code:is_loaded(Module) of
    {file, _} -> true;
    false -> false
  end.

get_name(Module, Names) ->
  case has_name(Module, Names) of
    true ->
      maps:get(Module, Names);
    false ->
      %% This module is load to evm but not recorded in `kapok_code`.
      Module
  end.

%load(Forms, )

%unload()
