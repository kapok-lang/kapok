%% symbol table for compilation
-module(kapok_symbol_table).
-behaviour(gen_server).
-export([add_namespace/1,
         add_fap/5,
         remove_fap/5,
         add_def/7,
         add_alias/3,
         add_form/2,
         add_suspended_def_clause/3,
         remove_suspended_def_clause/2,
         namespace_suspended_def_clause/1,
         namespace_locals/1,
         namespace_funs/1,
         namespace_macros/1,
         namespace_forms/4]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-include("kapok.hrl").

%% Public API

add_namespace(Namespace) ->
  gen_server:call(?MODULE, {add_ns, Namespace}).

add_fap(Namespace, Kind, Fun, Arity, ParameterType) ->
  gen_server:call(?MODULE, {add_fap, Namespace, Kind, Fun, Arity, ParameterType}).

remove_fap(Namespace, Kind, Fun, Arity, ParameterType) ->
  gen_server:call(?MODULE, {remove_fap, Namespace, Kind, Fun, Arity, ParameterType}).

add_def(Namespace, Kind, Fun, Arity, ParameterType, Meta, Clause) ->
  gen_server:call(?MODULE, {add_def, Namespace, Kind, Fun, Arity, ParameterType, Meta, Clause}).

add_alias(Namespace, Alias, Original) ->
  gen_server:call(?MODULE, {add_alias, Namespace, Alias, Original}).

add_form(Namespace, Form) ->
  gen_server:call(?MODULE, {add_form, Namespace, Form}).

add_suspended_def_clause(Namespace, FA, ClauseArgs) ->
  gen_server:call(?MODULE, {add_suspended_def_clause, Namespace, FA, ClauseArgs}).

remove_suspended_def_clause(Namespace, FA) ->
  gen_server:call(?MODULE, {remove_suspended_def_clause, Namespace, FA}).

namespace_suspended_def_clause(Namespace) ->
  gen_server:call(?MODULE, {namespace_suspended_def_clause, Namespace}).

%% TODO remove this
namespace_locals(Namespace) ->
  gen_server:call(?MODULE, {namespace_locals, Namespace}).

namespace_funs(Namespace) ->
  gen_server:call(?MODULE, {namespace_funs, Namespace}).

namespace_macros(Namespace) ->
  gen_server:call(?MODULE, {namespace_macros, Namespace}).

namespace_forms(Namespace, ModuleName, Ctx, Options) ->
  gen_server:call(?MODULE, {namespace_forms, Namespace, ModuleName, Ctx, Options}).

%% gen_server API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, maps:new()}.

%% namespace
handle_call({add_ns, Namespace}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  {reply, ok, Map1};
%% fap
handle_call({add_fap, Namespace, Kind, Fun, Arity, ParameterType}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  FAP = {Fun, Arity, ParameterType},
  Map2 = add_def(Namespace, Kind, FAP, Map1),
  {reply, ok, Map2};
handle_call({remove_fap, Namespace, Kind, Fun, Arity, ParameterType}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  FAP = {Fun, Arity, ParameterType},
  Map2 = remove_def(Namespace, Kind, FAP, Map1),
  {reply, ok, Map2};

%% def
handle_call({add_def, Namespace, Kind, Fun, Arity, ParameterType, Meta, Clause}, _From, Map) ->
  %% TODO check duplication and conflict for defs
  Map1 = add_namespace_if_missing(Namespace, Map),
  FAP = {Fun, Arity, ParameterType},
  Map2 = add_def(Namespace, Kind, FAP, Map1),
  Map3 = add_export(Namespace, Kind, FAP, Map2),
  FA = {Fun, Arity},
  Map4 = add_def_clause(Namespace, Kind, FA, Meta, Clause, Map3),
  {reply, ok, Map4};
%% alias
handle_call({add_alias, Namespace, Alias, Original}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  %% TODO check duplication and conflict among aliases and defs
  {R, Map2} = add_alias(Namespace, Alias, Original, Map1),
  {reply, R, Map2};
%% form
handle_call({add_form, Namespace, Form}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  Map2 = add_into_kv(Namespace, 'forms', Form, Map1),
  {reply, ok, Map2};
%% pending def clause
handle_call({add_suspended_def_clause, Namespace, FA, ClauseArgs}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  NS = maps:get(Namespace, Map1),
  Key =  'suspended_def_clauses',
  Clauses = maps:get(Key, NS),
  Args = case orddict:find(FA, Clauses) of
           {ok, V} -> ordsets:add_element(ClauseArgs, V);
           error -> ordsets:from_list([ClauseArgs])
         end,
  Clauses1 = orddict:store(FA, Args, Clauses),
  NS1 = maps:update(Key, Clauses1, NS),
  Map2 = maps:update(Namespace, NS1, Map1),
  {reply, ok, Map2};

handle_call({remove_suspended_def_clause, Namespace, FA}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  NS = maps:get(Namespace, Map1),
  Key = 'suspended_def_clauses',
  Clauses = maps:get(Key, NS),
  Clauses1 = orddict:erase(FA, Clauses),
  NS1 = maps:update(Key, Clauses1, NS),
  Map2 = maps:update(Namespace, NS1, Map1),
  {reply, ok, Map2};

handle_call({namespace_suspended_def_clause, Namespace}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  Suspended = get_kv(Namespace, 'suspended_def_clauses', Map1),
  {reply, Suspended, Map1};

%% local functions and macros
handle_call({namespace_locals, Namespace}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  Aliases = get_kv(Namespace, 'aliases', Map1),
  FAList = ordsets:union([get_kv(Namespace, 'macros', Map), get_kv(Namespace, 'funs', Map)]),
  {reply, Aliases ++ FAList, Map1};
handle_call({namespace_funs, Namespace}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  FAList = get_kv(Namespace, 'funs', Map1),
  {reply, FAList, Map1};
handle_call({namespace_macros, Namespace}, _From, Map) ->
  Map1 = add_namespace_if_missing(Namespace, Map),
  FAList = get_kv(Namespace, 'macros', Map1),
  {reply, FAList, Map1};
handle_call({namespace_forms, Namespace, ModuleName, Ctx, Options}, _From, Map) ->
  R = case lists:member(ignore_suspended_def_clauses, Options) of
        false ->
          NS = maps:get(Namespace, Map),
          case maps:get('suspended_def_clauses', NS) of
            [] ->
              {Forms, Ctx1} = namespace_forms(Namespace, ModuleName, Ctx, Options, Map),
              {ok, Forms, Ctx1};
            Suspended ->
              {suspended, orddict:to_list(Suspended), Ctx}
          end;
        true ->
          {Forms, Ctx1} = namespace_forms(Namespace, ModuleName, Ctx, Options, Map),
          {ok, Forms, Ctx1}
      end,
  {reply, R, Map}.

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
    suspended_def_clauses => []}.

add_namespace_if_missing(Namespace, Map) ->
  case maps:is_key(Namespace, Map) of
    false -> maps:put(Namespace, new_namespace(), Map);
    true -> Map
  end.

add_def(Namespace, Kind, FAP, Map) when Kind == 'defn-' ->
  add_def(Namespace, 'defn', FAP, Map);
add_def(Namespace, Kind, FAP, Map) when Kind == 'defn' ->
  add_into_kv(Namespace, 'funs', FAP, Map);
add_def(Namespace, Kind, FAP, Map) when Kind == 'defmacro-' ->
  add_def(Namespace, 'defmacro', FAP, Map);
add_def(Namespace, Kind, FAP, Map) when Kind == 'defmacro' ->
  add_into_kv(Namespace, 'macros', FAP, Map).

remove_def(Namespace, Kind, FAP, Map) when Kind == 'defn-' ->
  remove_def(Namespace, 'defn', FAP, Map);
remove_def(Namespace, Kind, FAP, Map) when Kind == 'defn' ->
  remove_from_kv(Namespace, 'funs', FAP, Map);
remove_def(Namespace, Kind, FAP, Map) when Kind == 'defmacro-' ->
  remove_def(Namespace, 'defmacro', FAP, Map);
remove_def(Namespace, Kind, FAP, Map) when Kind == 'defmacro' ->
  remove_from_kv(Namespace, 'macros', FAP, Map).

add_export(_Namespace, Kind, _FAP, Map) when Kind == 'defn-' ->
  Map;
add_export(Namespace, Kind, FAP, Map) when Kind == 'defn' ->
  add_into_kv(Namespace, 'export_funs', FAP, Map);
add_export(_Namespace, Kind, _FAP, Map) when Kind == 'defmacro-' ->
  Map;
add_export(Namespace, Kind, FAP, Map) when Kind == 'defmacro' ->
  add_into_kv(Namespace, 'export_macros', FAP, Map).

is_exported(Namespace, 'funs', FAP, Map) ->
  ordsets:is_element(FAP, get_kv(Namespace, 'export_funs', Map));
is_exported(Namespace, 'macros', FAP, Map) ->
  ordsets:is_element(FAP, get_kv(Namespace, 'export_macros', Map)).

add_def_clause(Namespace, Kind, FA, Meta, Clause, Map) when Kind == 'defn-' ->
  add_def_clause(Namespace, 'defn', FA, Meta, Clause, Map);
add_def_clause(Namespace, Kind, FA, Meta, Clause, Map) when Kind == 'defmacro-' ->
  add_def_clause(Namespace, 'defmacro', FA, Meta, Clause, Map);
add_def_clause(Namespace, Kind, FA, Meta, Clause, Map) when Kind == 'defn'; Kind == 'defmacro' ->
  Key = 'defs',
  NS = maps:get(Namespace, Map),
  Defs = maps:get(Key, NS),
  Clauses1 = case orddict:find(FA, Defs) of
               {ok, Clauses} -> orddict:store(Meta, Clause, Clauses);
               error -> orddict:from_list([{Meta, Clause}])
             end,
  Defs1 = orddict:store(FA, Clauses1, Defs),
  NS1 = maps:update(Key, Defs1, NS),
  maps:update(Namespace, NS1, Map).

add_alias(Namespace, Alias, Original, Map) ->
  case gen_aliases(Namespace, 'funs', Alias, Original, Map) of
    {[], Map1} ->
      case gen_aliases(Namespace, 'macros', Alias, Original, Map1) of
        {[], Map2} ->
          {not_exist, Map2};
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

remove_from_kv(Namespace, Key, List, Map) when is_list(List) ->
  NS = maps:get(Namespace, Map),
  Values = maps:get(Key, NS),
  Values1 = ordsets:subtract(Values, ordsets:from_list(List)),
  NS1 = maps:update(Key, Values1, NS),
  maps:update(Namespace, NS1, Map);
remove_from_kv(Namespace, Key, Value, Map) ->
  NS = maps:get(Namespace, Map),
  Values = maps:get(Key, NS),
  Values1 = ordsets:del_element(Value, Values),
  NS1 = maps:update(Key, Values1, NS),
  maps:update(Namespace, NS1, Map).

get_kv(Namespace, Key, Map) ->
  NS = maps:get(Namespace, Map),
  maps:get(Key, NS).

namespace_forms(Namespace, ModuleName, Ctx, Options, Map) ->
  {InfoExports, InfoDefs, Ctx1} = gen_info_fun(Namespace, Ctx, Options, Map),
  NS = maps:get(Namespace, Map),
  FunExports = namespace_exports(NS, Options),
  FunDefs = namespace_defs(NS, Options),
  Exports = InfoExports ++ FunExports,
  Defs = orddict:merge(fun(_K, V1, _V2) -> V1 end, InfoDefs, FunDefs),
  TranslateFun = fun({Fun, Arity}, {Line, Clauses}, Acc) ->
                     [{function, Line, Fun, Arity, Clauses} | Acc]
                 end,
  DefForms = orddict:fold(TranslateFun, [], Defs),
  Line = 1,
  AttrModule = {attribute, Line, module, ModuleName},
  AttrExport = {attribute, Line, export, Exports},
  %% add other forms such as attributes
  OtherForms = get_kv(Namespace, 'forms', Map),
  %% The `module_info' functions definitions and exports are added automatically
  %% by the erlang compiler, so it's not necessary to manually add them
  %% in kapok compiler.
  Forms = [AttrModule, AttrExport] ++ OtherForms ++ DefForms,
  {Forms, Ctx1}.

namespace_exports(NS, Options) ->
  GetExport = fun({F, A, _P}) -> {F, A} end,
  Functions = ordsets:from_list(lists:map(GetExport, maps:get('export_funs', NS))),
  Key = case lists:member(export_all_macro, Options) of
          true -> 'macros';
          false -> 'export_macros'
        end,
  Macros = ordsets:from_list(lists:map(GetExport, maps:get(Key, NS))),
  Exports = ordsets:union(Functions, Macros),
  Exports.

private_macros(NS) ->
  Exports = maps:get('export_macros', NS),
  All = maps:get('macros', NS),
  ordsets:subtract(All, Exports).

namespace_defs(NS, Options) ->
  GetMetaClauses = fun(Meta, Clause, {nil, Clauses}) -> {Meta, [Clause | Clauses]};
                      (_Meta, Clause, {Meta, Clauses}) -> {Meta, [Clause | Clauses]}
                   end,
  IterateDefs = fun(FA, MetaClauses, Acc) ->
                    {Meta, ReversedClauses} = orddict:fold(GetMetaClauses, {nil, []}, MetaClauses),
                    Line = ?line(Meta),
                    Clauses = lists:reverse(ReversedClauses),
                    orddict:store(FA, {Line, Clauses}, Acc)
                end,
  AllDefs = orddict:fold(IterateDefs, orddict:new(), maps:get('defs', NS)),
  Defs = case lists:member(strip_private_macro, Options) of
           true ->
             PrivateMacros = private_macros(NS),
             case PrivateMacros of
               [] ->
                 AllDefs;
               _ ->
                 Filter = fun ({F, A}, _) ->
                              Match = fun ({PF, PA, _PP}) when PF == F, PA == A -> true;
                                          (_) -> false
                                      end,
                              case ordsets:filter(Match, PrivateMacros) of
                                [] -> true;
                                _ -> false
                              end
                          end,
                 orddict:filter(Filter, AllDefs)
             end;
           false ->
             AllDefs
         end,
  %% insert aliases defs
  Aliases = maps:get('aliases', NS),
  AliasesDefs = lists:foldl(fun({Alias, {F, A, _}}, Acc) ->
                                {ok, MetaClauses} = orddict:find({F, A}, Defs),
                                orddict:store({Alias, A}, MetaClauses, Acc)
                            end,
                            orddict:new(),
                            Aliases),
  %% append aliases defs into head since it's much shorter in general.
  orddict:merge(fun(_K, V1, _V2) -> V1 end, Defs, AliasesDefs).

%% generate the `__info__' function for kapok.
gen_info_fun(Namespace, Ctx, Options, Map) ->
  NS = maps:get(Namespace, Map),
  %% The `module_info' functions definitions and exports are automatically added by
  %% the erlang compiler, their exports need to be added to the kapok exported functions list.
  ModuleInfoFunctions = ordsets:from_list([{'module_info', 0, 'normal'},
                                           {'module_info', 1, 'normal'}]),
  Functions = ordsets:union(ModuleInfoFunctions, maps:get(export_funs, NS)),
  Macros = case lists:member(export_all_macro, Options) of
             true -> maps:get(macros, NS);
             false -> maps:get(export_macros, NS)
           end,
  Line = 1,
  {TFunctions, Ctx1} = kapok_trans:translate(kapok_trans:quote([{line, Line}], Functions), Ctx),
  {TMacros, Ctx2} = kapok_trans:translate(kapok_trans:quote([{line, Line}], Macros), Ctx1),
  FA = {'__info__', 1},
  Exports = [FA],
  Defs = orddict:from_list([{FA,
                             {Line, [{clause, Line, [{atom, Line, 'functions'}], [], [TFunctions]},
                                     {clause, Line, [{atom, Line, 'macros'}], [], [TMacros]}]}}]),
  {Exports, Defs, Ctx2}.
