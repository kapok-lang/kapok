%% symbol table for compilation
-module(kapok_symbol_table).
-behaviour(gen_server).
-export([add_namespace/1,
         add_def_clause/5,
         add_export/5,
         add_alias/3,
         add_local/4,
         add_form/2,
         add_info_fun/2,
         namespace_defs/1,
         namespace_exports/1,
         namespace_aliases_defs/2,
         namespace_locals/1,
         get_local/2]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-include("kapok.hrl").

%% Public API

add_namespace(Namespace) ->
  gen_server:call(?MODULE, {add_ns, Namespace}).

add_def_clause(Namespace, Kind, Fun, Arity, Clause)
    when Kind == 'defn'; Kind == 'defn-'; Kind == 'defmacro' ->
  gen_server:call(?MODULE, {add_def_clause, Namespace, Kind, Fun, Arity, Clause}).

add_export(Namespace, Kind, Fun, Arity, ParameterType) ->
  gen_server:call(?MODULE, {add_export, Namespace, Kind, Fun, Arity, ParameterType}).

add_alias(Namespace, Alias, Original) ->
  gen_server:call(?MODULE, {add_defalias, Namespace, Alias, Original}).

add_local(Namespace, Fun, Arity, ParameterType) ->
  gen_server:call(?MODULE, {add_local, Namespace, Fun, Arity, ParameterType}).

add_form(Namespace, Form) ->
  gen_server:call(?MODULE, {add_form, Namespace, Form}).

add_info_fun(Namespace, Env) ->
  gen_server:call(?MODULE, {add_info_fun, Namespace, Env}).

namespace_defs(Namespace) ->
  gen_server:call(?MODULE, {namespace_defs, Namespace}).

namespace_aliases_defs(Namespace, Defs) ->
  gen_server:call(?MODULE, {namespace_aliases_defs, Namespace, Defs}).

namespace_exports(Namespace) ->
  gen_server:call(?MODULE, {namespace_exports, Namespace}).

namespace_locals(Namespace) ->
  gen_server:call(?MODULE, {namespace_locals, Namespace}).

get_local(Namespace, Local) ->
  gen_server:call(?MODULE, {get_local, Namespace, Local}).

%% gen_server API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, maps:new()}.

% namespace
handle_call({add_ns, Namespace}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  {reply, ok, Map1};
% export
handle_call({add_export, Namespace, Kind, _Fun, _Arity, _ParameterType}, _From, Map)
    when Kind == 'defn-'; Kind == 'defalias-' ->
  %% don't export for namespace private function definitions
  Map1 = add_namespace_if_missing(Namespace, Map),
  {reply, ok, Map1};
handle_call({add_export, Namespace, Kind, Fun, Arity, ParameterType}, _From, Map)
    when Kind == 'defn'; Kind == 'defalias' ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  Map2 = add_export(export_fun, Namespace, Fun, Arity, ParameterType, Map1),
  {reply, ok, Map2};
handle_call({add_export, Namespace, Kind, Fun, Arity, ParameterType}, _From, Map)
    when Kind == 'defmacro' ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  Map2 = add_export(export_macro, Namespace, Fun, Arity, ParameterType, Map1),
  {reply, ok, Map2};
handle_call({add_def_clause, Namespace, Kind, Fun, Arity, Clause}, _From, Map) ->
  Key = case Kind of
          defn -> Kind;
          'defn-' -> 'defn';
          defmacro -> Kind
        end,
  Map1 = add_namespace_if_missing(Namespace, Map),
  Map2 = add_def_clause(Key, Namespace, Fun, Arity, Clause, Map1),
  {reply, ok, Map2};
handle_call({add_defalias, Namespace, Alias, Original}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  NS = maps:get(Namespace, Map1),
  Aliases = maps:get(defalias, NS),
  Aliases1 = case Original of
               {Fun, Arity} -> ordsets:add_element({Alias, Fun, Arity}, Aliases);
               Fun -> ordsets:add_element({Alias, Fun}, Aliases)
             end,
  NS1 = maps:update(defalias, Aliases1, NS),
  Map2 = maps:update(Namespace, NS1, Map1),
  {reply, ok, Map2};
handle_call({add_local, Namespace, Fun, Arity, ParameterType}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  NS = maps:get(Namespace, Map1),
  Locals = maps:get(local, NS),
  Locals1 = ordsets:add_element({Fun, Arity, ParameterType}, Locals),
  NS1 = maps:update(local, Locals1, NS),
  Map2 = maps:update(Namespace, NS1, Map1),
  {reply, ok, Map2};
handle_call({add_form, Namespace, Form}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  NS = maps:get(Namespace, Map1),
  Forms = maps:get(form, NS),
  Forms1 = ordsets:add_element(Form, Forms),
  NS1 = maps:update(form, Forms1, NS),
  Map2 = maps:update(Namespace, NS1, Map1),
  {reply, ok, Map2};
handle_call({add_info_fun, Namespace, Env}, _From, Map) ->
  NS = maps:get(Namespace, Map),
  Functions = maps:get(export_fun, NS),
  Macros = maps:get(export_macro, NS),
  {TFunctions, Env1} = kapok_trans:translate(kapok_trans:quote([], Functions), Env),
  {TMacros, Env2} = kapok_trans:translate(kapok_trans:quote([], Macros), Env1),
  Map1 = add_export(export_fun, Namespace, '__info__', 1, 'normal', Map),
  Map2 = add_def_clause(defn, Namespace, '__info__', 1,
                        {clause, 0, [{atom,0,'functions'}], [], [TFunctions]}, Map1),
  Map3 = add_def_clause(defn, Namespace, '__info__', 1,
                        {clause, 0, [{atom,0,'macros'}], [], [TMacros]}, Map2),
  {reply, Env2, Map3};
handle_call({namespace_defs, Namespace}, _From, Map) ->
  NS = maps:get(Namespace, Map),
  Functions = maps:get(defn, NS),
  Macros = maps:get(defmacro, NS),
  %% TODO macro will shadow function if there is any overrides
  Defs = orddict:merge(fun(_K, _V1, V2) -> V2 end, Functions, Macros),
  {reply, lists:reverse(Defs), Map};
handle_call({namespace_exports, Namespace}, _From, Map) ->
  NS = maps:get(Namespace, Map),
  {ExportFunctions, ExportMacros} = module_exports(NS),
  Exports = ordsets:union(ExportFunctions, ExportMacros),
  {reply, lists:reverse(Exports), Map};
handle_call({namespace_aliases_defs, Namespace, Defs}, _From, Map) ->
  NS = maps:get(Namespace, Map),
  Aliases = maps:get(defalias, NS),
  AliasesDefs = lists:foldl(fun({Alias, F, A}, Acc) ->
                                {ok, Clauses} = orddict:find({F, A}, Defs),
                                orddict:store({Alias, A}, Clauses, Acc);
                               ({Alias, F}, Acc)->
                                L = orddict:filter(fun({F1, _A1}, _Clauses) -> F == F1 end, Defs),
                                orddict:fold(fun({_F, A}, Clauses, Acc1) ->
                                                 orddict:store({Alias, A}, Clauses, Acc1)
                                             end,
                                             Acc,
                                             L)
                            end,
                            orddict:new(),
                            Aliases),
  {reply, lists:reverse(AliasesDefs), Map};
handle_call({namespace_locals, Namespace}, _From, Map) ->
  NS = maps:get(Namespace, Map),
  Locals = maps:get(local, NS),
  {reply, Locals, Map};
handle_call({get_local, Namespace, {Fun, Arity}}, _From, Map) ->
  NS = maps:get(Namespace, Map),
  Locals = maps:get(local, NS),
  Match = ordsets:filter(fun({F, A, _P}) -> (F == Fun) andalso (A == Arity) end, Locals),
  {reply, Match, Map};
handle_call({get_local, Namespace, Fun}, _From, Map) ->
  NS = maps:get(Namespace, Map),
  Locals = maps:get(local, NS),
  Match = ordsets:filter(fun({F, _A, _P}) -> F == Fun end, Locals),
  {reply, Match, Map}.

handle_cast({_, _}, Map) ->
  {noreply, Map}.

handle_info({_, _}, Map) ->
  {noreply, Map}.

terminate(_Reason, _Map) ->
  ok.

code_change(_OldVsn, Map, _Extra) ->
  {ok, Map}.

%% helper functions

new_namespace() ->
  #{export_fun => [],
    export_macro => [],
    defn => [],
    'defn-' => [],
    defmacro => [],
    defalias => [],
    local => [],
    form => []}.

add_namespace_if_missing(Namespace, Map) ->
  case maps:is_key(Namespace, Map) of
    false -> maps:put(Namespace, new_namespace(), Map);
    true -> Map
  end.

add_export(Key, Namespace, Fun, Arity, ParameterType, Map) ->
  NS = maps:get(Namespace, Map),
  Exports = maps:get(Key, NS),
  Exports1 = ordsets:add_element({Fun, Arity, ParameterType}, Exports),
  NS1 = maps:update(Key, Exports1, NS),
  maps:update(Namespace, NS1, Map).

module_exports(NS) ->
  GetExport = fun({F, A, _P}) -> {F, A} end,
  Functions = ordsets:from_list(lists:map(GetExport, maps:get(export_fun, NS))),
  Macros = ordsets:from_list(lists:map(GetExport, maps:get(export_macro, NS))),
  {Functions, Macros}.

add_def_clause(Key, Namespace, Fun, Arity, Clause, Map) ->
  NS = maps:get(Namespace, Map),
  Def = maps:get(Key, NS),
  FA = {Fun, Arity},
  Clauses1 = case orddict:find(FA, Def) of
               {ok, Clauses} -> [Clause | Clauses];
               error -> [Clause]
             end,
  Def1 = orddict:store(FA, Clauses1, Def),
  NS1 = maps:update(Key, Def1, NS),
  maps:update(Namespace, NS1, Map).
