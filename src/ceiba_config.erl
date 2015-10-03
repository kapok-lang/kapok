%% read/write configuration
-module(ceiba_config).
-compile({no_auto_import, [get/1]}).
-export([new/1, delete/1, put/2, get/1, update/2, get_and_put/2]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).
-behaviour(gen_server).

%% public api

new(Options) ->
    Tid = ets:new(?MODULE, [named_tuble, public, {read_concurrency, true}]),
    true = ets:insert_new(?MODULE, Options),
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

get_and_put(Key, Value) ->
    gen_server:call(?MODULE, {get_and_put, Key, Value}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

%% gen_server api

init(Tid) ->
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

code_change(_OldVersion, Tid, _Extra) ->
    {ok, Tid}.

terminate(_Reason, _Tid) ->
    ok.

