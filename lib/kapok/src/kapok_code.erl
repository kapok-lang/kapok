%% Code server for kapok compilation.
-module(kapok_code).
-behaviour(gen_server).
-export([is_loaded/2,
         load/3,
         call/6]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Public API
is_loaded(Module, Env) ->
  gen_server:call(?MODULE, {is_loaded, Module, Env}).

load(Module, Binary, Env) ->
  gen_server:call(?MODULE, {load, Module, Binary, Env}).

call(Meta, Module, Fun, Arity, Args, Env) ->
  get_server:call(?MODULE, {call, Meta, Module, Fun, Arity, Args, Env}).


%% gen_server API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, maps:new()}.

handle_call({is_loaded, _Module, _Env}, _From, Map) ->
  %% TODO add impl
  {reply, ok, Map};
handle_call({load, _Module, _Binary, _Env}, _From, Map) ->
  %% TODO add impl
  {reply, ok, Map};
handle_call({call, _Meta, _Module, _Fun, _Arity, _Args, _Env}, _From, Map) ->
  %% TODO add impl
  {reply, ok, Map}.

handle_cast({_, _}, Map) ->
  {noreply, Map}.

handle_info({_, _}, Map) ->
  {noreply, Map}.

terminate(_Reason, _Map) ->
  ok.

code_change(_OldVsn, Map, _Extra) ->
  {ok, Map}.
