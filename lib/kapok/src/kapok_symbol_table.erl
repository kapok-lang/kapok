%% symbol table for compilation
-module(kapok_symbol_table).
-behaviour(gen_server).
-export([add_namespace/1,
         add_def/6,
         add_alias/3,
         add_form/2,
         namespace_locals/1,
         namespace_funs/1,
         namespace_macros/1,
         namespace_forms/3]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-include("kapok.hrl").

%% Public API

add_namespace(Namespace) ->
  gen_server:call(?MODULE, {add_ns, Namespace}).

add_def(Namespace, Kind, Fun, Arity, ParameterType, Clause) ->
  gen_server:call(?MODULE, {add_def, Namespace, Kind, Fun, Arity, ParameterType, Clause}).

add_alias(Namespace, Alias, Original) ->
  gen_server:call(?MODULE, {add_alias, Namespace, Alias, Original}).

add_form(Namespace, Form) ->
  gen_server:call(?MODULE, {add_form, Namespace, Form}).

%% TODO remove this
namespace_locals(Namespace) ->
  gen_server:call(?MODULE, {namespace_locals, Namespace}).

namespace_funs(Namespace) ->
  gen_server:call(?MODULE, {namespace_funs, Namespace}).

namespace_macros(Namespace) ->
  gen_server:call(?MODULE, {namespace_macros, Namespace}).

namespace_forms(Namespace, ModuleName, Env) ->
  gen_server:call(?MODULE, {namespace_forms, Namespace, ModuleName, Env}).

%% gen_server API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, maps:new()}.

%% namespace
handle_call({add_ns, Namespace}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  {reply, ok, Map1};
%% def
handle_call({add_def, Namespace, Kind, Fun, Arity, ParameterType, Clause}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  FAP = {Fun, Arity, ParameterType},
  Map2 = add_def(Namespace, Kind, FAP, Map1),
  Map3 = add_export(Namespace, Kind, FAP, Map2),
  FA = {Fun, Arity},
  Map4 = add_def_clause(Namespace, Kind, FA, Clause, Map3),
  {reply, ok, Map4};
%% alias
handle_call({add_alias, Namespace, Alias, Original}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  {R, Map2} = add_alias(Namespace, Alias, Original, Map1),
  {reply, R, Map2};
%% form
handle_call({add_form, Namespace, Form}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  Map2 = add_into_kv(Namespace, 'forms', Form, Map1),
  {reply, ok, Map2};
%% local functions and macros
handle_call({namespace_locals, Namespace}, _From, Map) ->
  Aliases = get_kv(Namespace, 'aliases', Map),
  MFAList = ordsets:union([get_kv(Namespace, 'macros', Map), get_kv(Namespace, 'funs', Map)]),
  {reply, Aliases ++ MFAList, Map};
handle_call({namespace_funs, Namespace}, _From, Map) ->
  MFAList = get_kv(Namespace, 'funs', Map),
  {reply, MFAList, Map};
handle_call({namespace_macros, Namespace}, _From, Map) ->
  MFAList = get_kv(Namespace, 'macros', Map),
  {reply, MFAList, Map};
handle_call({namespace_forms, Namespace, ModuleName, Env}, _From, Map) ->
  {InfoExports, InfoDefs, Env1} = gen_info_fun(Namespace, Env, Map),
  NS = maps:get(Namespace, Map),
  FunExports = namespace_exports(NS),
  FunDefs = namespace_defs(NS),
  Exports = InfoExports ++ FunExports,
  Defs = InfoDefs ++ FunDefs,
  TranslateFun = fun({{Fun, Arity}, Clauses}, Acc) ->
                     [{function, 0, Fun, Arity, lists:reverse(Clauses)} | Acc]
                 end,
  DefForms = lists:foldl(TranslateFun, [], Defs),
  AttrModule = {attribute, 0, module, ModuleName},
  AttrExport = {attribute, 0, export, Exports},
  Forms = [AttrModule, AttrExport | DefForms],
  {reply, {Forms, Env1}, Map}.

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
  #{export_funs => [],
    export_macros => [],
    funs => [],
    macros => [],
    aliases => [],
    defs => [],
    locals => [],
    forms => [],
    suspended_defs => []}.

add_namespace_if_missing(Namespace, Map) ->
  case maps:is_key(Namespace, Map) of
    false -> maps:put(Namespace, new_namespace(), Map);
    true -> Map
  end.

add_def(Namespace, Kind, FAP, Map) when Kind == 'defn-' ->
  add_def(Namespace, 'defn', FAP, Map);
add_def(Namespace, Kind, FAP, Map) when Kind == 'defn' ->
  add_into_kv(Namespace, 'funs', FAP, Map);
add_def(Namespace, Kind, FAP, Map) when Kind == 'defmacro' ->
  add_into_kv(Namespace, 'macros', FAP, Map).

add_export(_Namespace, Kind, _FAP, Map) when Kind == 'defn-' ->
  Map;
add_export(Namespace, Kind, FAP, Map) when Kind == 'defn' ->
  add_into_kv(Namespace, 'export_funs', FAP, Map);
add_export(Namespace, Kind, FAP, Map) when Kind == 'defmacro' ->
  add_into_kv(Namespace, 'export_macros', FAP, Map).

is_exported(Namespace, 'funs', FAP, Map) ->
  ordsets:is_element(FAP, get_kv(Namespace, 'export_funs', Map));
is_exported(Namespace, 'macros', FAP, Map) ->
  ordsets:is_element(FAP, get_kv(Namespace, 'export_macros', Map)).

add_def_clause(Namespace, Kind, FA, Clause, Map) when Kind == 'defn-' ->
  add_def_clause(Namespace, 'defn', FA, Clause, Map);
add_def_clause(Namespace, Kind, FA, Clause, Map) when Kind == 'defn'; Kind == 'defmacro' ->
  Key = 'defs',
  NS = maps:get(Namespace, Map),
  Defs = maps:get(Key, NS),
  Clauses1 = case orddict:find(FA, Defs) of
               {ok, Clauses} -> [Clause | Clauses];
               error -> [Clause]
             end,
  Defs1 = orddict:store(FA, Clauses1, Defs),
  NS1 = maps:update(Key, Defs1, NS),
  maps:update(Namespace, NS1, Map).

add_alias(Namespace, Alias, Original, Map) ->
  case gen_aliases(Namespace, 'funs', Alias, Original, Map) of
    {[], Map1} ->
      case gen_aliases(Namespace, 'macros', Alias, Original, Map1) of
        {[], Map2} ->
          {noexist, Map2};
        {L, Map2} ->
          {ok, add_into_kv(Namespace, 'aliases', L, Map2)}
      end;
    {L, Map1} ->
      {ok, add_into_kv(Namespace, 'aliases', L, Map1)}
  end.

%% TODO The impl is similar with kapok_dispatch:filter_exports_by('rename').
gen_aliases(Namespace, Key, Alias, Original, Map) ->
  lists:foldl(fun({F, A, P} = FAP, {Acc, M}) ->
                  case Original of
                    {Fun, Arity} ->
                      if F == Fun andalso A == Arity ->
                          M1 = add_export_alias(Namespace, Key, Alias, FAP, M),
                          {[{Alias, {F, A, P}} | Acc], M1};
                         true ->
                          {Acc, M}
                      end;
                    Fun ->
                      if F == Fun ->
                          M1 = add_export_alias(Namespace, Key, Alias, FAP, M),
                          {[{Alias, {F, A, P}} | Acc], M1};
                         true ->
                          {Acc, M}
                      end
                  end
              end,
              {[], Map},
              get_kv(Namespace, Key, Map)).

%% Check whether this FAP is exported.
%% Export Alias when the original name is exported.
add_export_alias(Namespace, Key, Alias, {_, A, P} = FAP, Map) ->
  case is_exported(Namespace, Key, FAP, Map) of
    true ->
      Kind = case Key of
               'funs' -> 'defn';
               'macros' -> 'defmacro'
             end,
      add_export(Namespace, Kind, {Alias, A, P}, Map);
    false ->
      Map
  end.

add_into_kv(Namespace, Key, List, Map) when is_list(List) ->
  NS = maps:get(Namespace, Map),
  Values = maps:get(Key, NS),
  Values1 = ordsets:union([ordsets:from_list(List), Values]),
  NS1 = maps:update(Key, Values1, NS),
  maps:update(Namespace, NS1, Map);
add_into_kv(Namespace, Key, Value, Map) ->
  NS = maps:get(Namespace, Map),
  Values = maps:get(Key, NS),
  Values1 = ordsets:add_element(Value, Values),
  NS1 = maps:update(Key, Values1, NS),
  maps:update(Namespace, NS1, Map).

get_kv(Namespace, Key, Map) ->
  NS = maps:get(Namespace, Map),
  maps:get(Key, NS).

namespace_exports(NS) ->
  GetExport = fun({F, A, _P}) -> {F, A} end,
  Functions = ordsets:from_list(lists:map(GetExport, maps:get('export_funs', NS))),
  Macros = ordsets:from_list(lists:map(GetExport, maps:get('export_macros', NS))),
  Exports = ordsets:union(Functions, Macros),
  Exports.

namespace_defs(NS) ->
  Defs = maps:get('defs', NS),
  %% insert aliases defs
  Aliases = maps:get('aliases', NS),
  AliasesDefs = lists:foldl(fun({Alias, {F, A, _}}, Acc) ->
                                {ok, Clauses} = orddict:find({F, A}, Defs),
                                orddict:store({Alias, A}, Clauses, Acc)
                            end,
                            orddict:new(),
                            Aliases),
  %% append aliases defs into head since it's much shorter in general.
  AliasesDefs ++ Defs.

gen_info_fun(Namespace, Env, Map) ->
  NS = maps:get(Namespace, Map),
  Functions = maps:get(export_funs, NS),
  Macros = maps:get(export_macros, NS),
  {TFunctions, Env1} = kapok_trans:translate(kapok_trans:quote([], Functions), Env),
  {TMacros, Env2} = kapok_trans:translate(kapok_trans:quote([], Macros), Env1),
  FA = {'__info__', 1},
  Exports = [FA],
  Defs = orddict:from_list([{FA,
                             [{clause, 0, [{atom,0,'functions'}], [], [TFunctions]},
                              {clause, 0, [{atom,0,'macros'}], [], [TMacros]}]}]),
  {Exports, Defs, Env2}.
