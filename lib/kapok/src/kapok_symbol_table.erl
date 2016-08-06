%% symbol table for compilation
-module(kapok_symbol_table).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-include("kapok.hrl").

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, maps:new()}.

handle_call({_, _}, _From, Map) ->
  {reply, ok, Map}.

handle_cast({_, _}, Map) ->
  {noreply, Map}.

handle_info({_, _}, Map) ->
  {noreply, Map}.

terminate(_Reason, _Map) ->
  ok.

code_change(_OldVsn, Map, _Extra) ->
  {ok, Map}.

