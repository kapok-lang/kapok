-module(kapok_config).
-compile({no_auto_import, [get/1]}).
-export([get_compiler_opt/1]).
-export([new/1, delete/1, put/2, get/1, update/2, update_in/2, get_and_put/2]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
-behaviour(gen_server).

%% Helper APIs which wrap public APIs

get_compiler_opt(Key) ->
  Options = kapok_config:get(compiler_options),
  case lists:keyfind(Key, 1, Options) of
    false -> false;
    {Key, Value} -> Value
  end.

%% Public API

new(Opts) ->
  Tid = ets:new(?MODULE, [named_table, public, {read_concurrency, true}]),
  true = ets:insert_new(?MODULE, Opts),
  Tid.

delete(?MODULE) ->
  ets:delete(?MODULE).

put(Key, Value) ->
  gen_server:call(?MODULE, {put, Key, Value}).

get(Key) ->
  case ets:lookup(?MODULE, Key) of
    [{_, Value}] -> Value;
    [] -> nil
  end.

update(Key, Fun) ->
  gen_server:call(?MODULE, {update, Key, Fun}).

update_in(Key, Orddict) when is_list(Orddict) ->
  Merge = fun(_, _, Value) -> Value end,
  Update = fun(Old) -> orddict:merge(Merge, Old, Orddict) end,
  kapok_config:update(Key, Update).

get_and_put(Key, Value) ->
  get_server:call(?MODULE, {get_and_put, Key, Value}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

%% gen_server API

init(Tid) ->
  %% ets table must be writable
  public = ets:info(Tid, protection),
  {ok, Tid}.

handle_call({put, Key, Value}, _From, Tid) ->
  ets:insert(Tid, {Key, Value}),
  {reply, ok, Tid};
handle_call({update, Key, Fun}, _From, Tid) ->
  Value = Fun(get(Key)),
  ets:insert(Tid, {Key, Value}),
  {reply, Value, Tid};
handle_call({get_and_put, Key, Value}, _From, Tid) ->
  OldValue = get(Key),
  ets:insert(Tid, {Key, Value}),
  {reply, OldValue, Tid}.

handle_cast(Cast, Tid) ->
  {stop, {bad_cast, Cast}, Tid}.

handle_info(_Msg, Tid) ->
  {noreply, Tid}.

code_change(_OldVsn, Tid, _Extra) ->
  {ok, Tid}.

terminate(_Reason, _Tid) ->
  ok.
