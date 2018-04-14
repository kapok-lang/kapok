%% symbol table for compilation
-module(kapok_symbol_table).
-behaviour(gen_server).
-export([new_namespace/1,
         is_namespace_defined/1,
         new_def/7,
         new_alias/4,
         new_form/2,
         new_suspended_def_clause/3,
         check_suspended_def_clauses/3,
         cleanup_fap_alias/4,
         namespace_macros/1,
         namespace_locals/1,
         namespace_forms/4]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-import(kapok_utils, [meta_order/1]).
-include("kapok.hrl").

%% Public API

new_namespace(Namespace) ->
  gen_server:call(?MODULE, {new_ns, Namespace}).

is_namespace_defined(Namespace) ->
  gen_server:call(?MODULE, {is_ns_defined, Namespace}).

new_def(Namespace, Kind, Fun, Arity, ParameterType, Meta, Clause) ->
  gen_server:call(?MODULE, {new_def, Namespace, Kind, Fun, Arity, ParameterType, Meta, Clause}).

new_alias(Namespace, Alias, Original, Meta) ->
  gen_server:call(?MODULE, {new_alias, Namespace, Alias, Original, Meta}).

new_form(Namespace, Form) ->
  gen_server:call(?MODULE, {new_form, Namespace, Form}).

new_suspended_def_clause(Namespace, UnresolvedFA, ClauseArgs) ->
  gen_server:call(?MODULE, {new_suspended_def_clause, Namespace, UnresolvedFA, ClauseArgs}).

check_suspended_def_clauses(Namespace, Kind, FAPList) ->
  gen_server:call(?MODULE, {check_suspended_def_clauses, Namespace, Kind, FAPList}).

cleanup_fap_alias(Namespace, Kind, FAPList, AliasFAPList) ->
  gen_server:call(?MODULE, {cleanup_fap_alias, Namespace, Kind, FAPList, AliasFAPList}).

namespace_macros(Namespace) ->
  gen_server:call(?MODULE, {namespace_macros, Namespace}).

namespace_locals(Namespace) ->
  gen_server:call(?MODULE, {namespace_locals, Namespace}).

namespace_forms(Namespace, ModuleName, Ctx, Options) ->
  case gen_server:call(?MODULE, {namespace_forms, Namespace, ModuleName, Ctx, Options}) of
    {invalid_alias, [{{Alias, Original}, Meta} | _T], Ctx1}  ->
      AliasDesc = case Original of
                    {Fun, Arity} -> io_lib:format("(~s ~B)", [Fun, Arity]);
                    Fun -> io_lib:format("~s", [Fun])
                  end,
      kapok_error:compile_error(Meta, ?m(Ctx1, file),
                                "invalid alias ~s because the original ~s does not exist",
                                [Alias, AliasDesc]);
    {unresolved, {UnresolvedFA, FAMeta}, Ctx1} ->
      kapok_error:compile_error(FAMeta, ?m(Ctx1, file), "unresolved local call: ~p", [UnresolvedFA]);
    {ok, Forms, Ctx1} ->
      {Forms, Ctx1}
  end.

%% gen_server API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, maps:new()}.

%% namespace
handle_call({new_ns, Namespace}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  {reply, ok, Map1};

handle_call({is_ns_defined, Namespace}, _From, Map) ->
  R = namespace_exist(Map, Namespace),
  {reply, R, Map};

%% def
handle_call({new_def, Namespace, Kind, Fun, Arity, ParameterType, Meta, Clause}, _From, Map) ->
  %% TODO check duplication and conflict for defs
  Map1 = add_namespace_if_missing(Map, Namespace),
  FAP = {Fun, Arity, ParameterType},
  Map2 = add_def(Map1, Namespace, Kind, FAP),
  Map3 = add_export(Map2, Namespace, Kind, FAP),
  Map4 = check_and_add_alias(Map3, Namespace, Kind, FAP),
  FA = {Fun, Arity},
  Map5 = add_def_clause(Map4, Namespace, Kind, FA, Meta, Clause),
  {reply, ok, Map5};

%% alias
handle_call({new_alias, Namespace, Alias, Original, Meta}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  %% TODO check duplication and conflict among aliases and defs
  %% TODO check duplication for AliasKey
  Map2 = add_into_kv_dict(Map1, Namespace, 'aliases', {Alias, Original}, Meta),
  %% check whether there is any the existing function or macro which matches
  %% this alias
  Map3 = gen_aliases(Map2, Namespace, Alias, Original, Meta),
  {reply, ok, Map3};

%% form
handle_call({new_form, Namespace, Form}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  Map2 = add_into_kv_set(Map1, Namespace, 'forms', Form),
  {reply, ok, Map2};

%% suspended def clause
handle_call({new_suspended_def_clause, Namespace, UnresolvedFA, {_FAMeta, Args} = ClauseArgs}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  FAP = element(1, Args),
  Meta = element(3, Args),
  Map2 = add_suspended_def_clause(Map1, Namespace, UnresolvedFA, Meta, FAP, ClauseArgs),
  {reply, ok, Map2};

handle_call({check_suspended_def_clauses, Namespace, Kind, FAPList}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  Match = match_aliases(FAPList, get_kv(Map1, Namespace, 'aliases')),
  Key = case Kind of
          K1 when K1 == 'defn'; K1 == 'defn-' -> 'fun_aliases';
          K2 when K2 == 'defmacro'; K2 == 'defmacro-' -> 'macro_aliases'
        end,
  NewAliasFAPList = filter_aliases(Match, orddict:from_list(get_kv_set(Map1, Namespace, Key))),
  AllFAPList = NewAliasFAPList ++ FAPList,
  SuspendedClauses = get_kv(Map1, Namespace, 'suspended_def_clauses'),
  Folder = fun(FA, D, {Suspended, M} = Acc) ->
               case kapok_dispatch:filter_fa(FA, AllFAPList) of
                 [] ->
                   Acc;
                 _ ->
                   M1 = remove_suspended_def_clause(M, Namespace, FA),
                   {maps:put(FA, D, Suspended), M1}
               end
           end,
  {ToHandle, Map2} = maps:fold(Folder, {maps:new(), Map1}, SuspendedClauses),
  Map4 = case maps:size(ToHandle) of
           0 ->
             Map2;
           _ ->
             Map3 = lists:foldl(fun(FAP, M) -> add_def(M, Namespace, Kind, FAP) end,
                                Map2,
                                FAPList),
             lists:foldl(fun(AliasFAP, M) -> add_alias(M, Namespace, Kind, AliasFAP) end,
                         Map3,
                         NewAliasFAPList)
         end,
  {reply, {ToHandle, NewAliasFAPList}, Map4};
%% fap alias
handle_call({cleanup_fap_alias, Namespace, Kind, FAPList, AliasFAPList}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  Map2 = lists:foldl(fun(FAP, M) -> remove_def(M, Namespace, Kind, FAP) end,
                     Map1,
                     FAPList),
  Map3 = lists:foldl(fun(AliasFAP, M) -> remove_alias(M, Namespace, Kind, AliasFAP) end,
                    Map2,
                    AliasFAPList),
  {reply, ok, Map3};

handle_call({namespace_macros, Namespace}, _From, Map) ->
  R = case namespace_exist(Map, Namespace) of
        true ->
          %% direct macro definitions
          Macros = get_kv_set(Map, Namespace, 'macros'),
          Aliases = get_kv_set(Map, Namespace, 'macro_aliases'),
          Aliases ++ Macros;
        false ->
          []
      end,
  {reply, R, Map};

handle_call({namespace_locals, Namespace}, _From, Map) ->
  R = case namespace_exist(Map, Namespace) of
        true ->
          Funs = get_kv(Map, Namespace, 'funs'),
          FunAliases = get_kv(Map, Namespace, 'fun_aliases'),
          Macros = get_kv(Map, Namespace, 'macros'),
          MacroAliases = get_kv(Map, Namespace, 'macro_aliases'),
          gb_sets:to_list(gb_sets:union([Funs, Macros, FunAliases, MacroAliases]));
        false ->
          []
      end,
  {reply, R, Map};

handle_call({namespace_forms, Namespace, ModuleName, Ctx, Options}, _From, Map) ->
  R = case namespace_exist(Map, Namespace) of
        true ->
          case lists:member(ignore_suspended_def_clauses, Options) of
            false ->
              Suspended = get_kv(Map, Namespace, 'suspended_def_clauses'),
              case maps:size(Suspended) of
                0 ->
                  case validate_aliases(Map, Namespace) of
                    {invalid_alias, _} = V ->
                      V;
                    ok ->
                      {Forms, Ctx1} = namespace_forms(Namespace, ModuleName, Ctx, Options, Map),
                      {ok, Forms, Ctx1}
                  end;
                _ ->
                  Dependencies = get_kv(Map, Namespace, 'fap_dependencies'),
                  UnresolvedFAInfo = find_unresolved_fa(Suspended, Dependencies),
                  {unresolved, UnresolvedFAInfo, Ctx}
              end;
            true ->
              {Forms, Ctx1} = namespace_forms(Namespace, ModuleName, Ctx, Options, Map),
              {ok, Forms, Ctx1}
          end;
        false ->
          []
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
  #{export_funs => gb_sets:new(),
    export_macros => gb_sets:new(),
    funs => gb_sets:new(),
    macros => gb_sets:new(),
    aliases => maps:new(),
    fun_aliases => gb_sets:new(),
    macro_aliases => gb_sets:new(),
    defs => maps:new(),
    forms => gb_sets:new(),
    suspended_def_clauses => maps:new(),
    fap_dependencies => maps:new(),
    order => 0}.

add_namespace_if_missing(Map, Namespace) ->
  case maps:is_key(Namespace, Map) of
    false -> maps:put(Namespace, new_namespace(), Map);
    true -> Map
  end.

namespace_exist(Map, Namespace) ->
  maps:is_key(Namespace, Map).

add_def(Map, Namespace, Kind, FAP) when Kind == 'defn-' ->
  add_def(Map, Namespace, 'defn', FAP);
add_def(Map, Namespace, Kind, FAP) when Kind == 'defn' ->
  add_into_kv_set(Map, Namespace, 'funs', FAP);
add_def(Map, Namespace, Kind, FAP) when Kind == 'defmacro-' ->
  add_def(Map, Namespace, 'defmacro', FAP);
add_def(Map, Namespace, Kind, FAP) when Kind == 'defmacro' ->
  add_into_kv_set(Map, Namespace, 'macros', FAP).

remove_def(Map, Namespace, Kind, FAP) when Kind == 'defn-' ->
  remove_def(Map, Namespace, 'defn', FAP);
remove_def(Map, Namespace, Kind, FAP) when Kind == 'defn' ->
  remove_from_kv_set(Map, Namespace, 'funs', FAP);
remove_def(Map, Namespace, Kind, FAP) when Kind == 'defmacro-' ->
  remove_def(Map, Namespace, 'defmacro', FAP);
remove_def(Map, Namespace, Kind, FAP) when Kind == 'defmacro' ->
  remove_from_kv_set(Map, Namespace, 'macros', FAP).

add_alias(Map, Namespace, Kind, AliasFAP) when Kind == 'defn-' ->
  add_alias(Map, Namespace, 'defn', AliasFAP);
add_alias(Map, Namespace, Kind, AliasFAP) when Kind == 'defn' ->
  add_into_kv_set(Map, Namespace, 'fun_aliases', AliasFAP);
add_alias(Map, Namespace, Kind, AliasFAP) when Kind == 'defmacro-' ->
  add_alias(Map, Namespace, 'defmacro', AliasFAP);
add_alias(Map, Namespace, Kind, AliasFAP) when Kind == 'defmacro' ->
  add_into_kv_set(Map, Namespace, 'macro_aliases', AliasFAP).

remove_alias(Map, Namespace, Kind, AliasFAP) when Kind == 'defn-' ->
  remove_alias(Map, Namespace, 'defn', AliasFAP);
remove_alias(Map, Namespace, Kind, AliasFAP) when Kind == 'defn' ->
  remove_alias(Map, Namespace, 'fun_aliases', AliasFAP);
remove_alias(Map, Namespace, Kind, AliasFAP) when Kind == 'defmacro-' ->
  remove_alias(Map, Namespace, 'defmacro', AliasFAP);
remove_alias(Map, Namespace, Kind, AliasFAP) when Kind == 'defmacro' ->
  remove_alias(Map, Namespace, 'macro_aliases', AliasFAP).

add_export(Map, _Namespace, Kind, _FAP) when Kind == 'defn-' ->
  Map;
add_export(Map, Namespace, Kind, FAP) when Kind == 'defn' ->
  add_into_kv_set(Map, Namespace, 'export_funs', FAP);
add_export(Map, _Namespace, Kind, _FAP) when Kind == 'defmacro-' ->
  Map;
add_export(Map, Namespace, Kind, FAP) when Kind == 'defmacro' ->
  add_into_kv_set(Map, Namespace, 'export_macros', FAP).

is_exported(Map, Namespace, 'defn', FAP) ->
  is_exported(maps:get(Namespace, Map), 'export_funs', FAP);
is_exported(Map, Namespace, 'defmacro', FAP) ->
  is_exported(maps:get(Namespace, Map), 'export_macros', FAP).

is_exported(NS, Key, FAP) ->
  gb_sets:is_element(FAP, maps:get(Key, NS)).

next_order(Namespace, Map) ->
  Key = 'order',
  Fun = fun(Order) -> Order + 1 end,
  Map1 = update_kv(Map, Namespace, Key, Fun),
  Next = get_kv(Map1, Namespace, Key),
  {Next, Map1}.

add_def_clause(Map, Namespace, Kind, FA, Meta, Clause) when Kind == 'defn-' ->
  add_def_clause(Map, Namespace, 'defn', FA, Meta, Clause);
add_def_clause(Map, Namespace, Kind, FA, Meta, Clause) when Kind == 'defmacro-' ->
  add_def_clause(Map, Namespace, 'defmacro', FA, Meta, Clause);
add_def_clause(Map, Namespace, Kind, FA, Meta, Clause) when Kind == 'defn'; Kind == 'defmacro' ->
  {Order, Map1} = case meta_order(Meta) of
                    0 -> next_order(Namespace, Map);
                    Value -> {Value, Map}
                  end,
  add_into_kv_dict_dict_set(Map1, Namespace, 'defs', FA, {Order, Meta}, Clause).

add_suspended_def_clause(Map, Namespace, UnresolvedFA, Meta, FAP, ClauseArgs) ->
  {Order, Map1} = case meta_order(Meta) of
                    0 -> next_order(Namespace, Map);
                    Value -> {Value, Map}
                  end,
  Map2 = add_into_kv_dict_dict(Map1, Namespace, 'suspended_def_clauses', UnresolvedFA, Order, ClauseArgs),
  add_into_kv_dict(Map2, Namespace, 'fap_dependencies', FAP, UnresolvedFA).

remove_suspended_def_clause(Map, Namespace, FA) ->
  Solved = get_kv_dict(Map, Namespace, 'suspended_def_clauses', FA),
  maps:fold(fun (_Order, {_Meta, Args}, M) ->
                FAP = element(1, Args),
                remove_from_kv_dict(M, Namespace, 'fap_dependencies', FAP)
            end,
            Map,
            Solved),
  remove_from_kv_dict(Map, Namespace, 'suspended_def_clauses', FA).

get_kv(Map, Namespace, Key) ->
  NS = maps:get(Namespace, Map),
  maps:get(Key, NS).

update_kv(Map, Namespace, Key, Fun) ->
  NS = maps:get(Namespace, Map),
  Value = maps:get(Key, NS),
  Value1 = Fun(Value),
  NS1 = maps:update(Key, Value1, NS),
  maps:update(Namespace, NS1, Map).

add_into_kv_set(Map, Namespace, Key, Value) ->
  Fun = fun(V) -> gb_sets:add_element(Value, V) end,
  update_kv(Map, Namespace, Key, Fun).

get_kv_set(Map, Namespace, Key) ->
  Set = get_kv(Map, Namespace, Key),
  gb_sets:to_list(Set).

add_into_kv_dict(Map, Namespace, Key, DictKey, DictValue) ->
  Fun = fun(V) -> maps:put(DictKey, DictValue, V) end,
  update_kv(Map, Namespace, Key, Fun).

get_kv_dict(Map, Namespace, Key, DictKey) ->
  Dict = get_kv(Map, Namespace, Key),
  maps:get(DictKey, Dict).

remove_from_kv_set(Map, Namespace, Key, Value) ->
  Fun = fun(V) -> gb_sets:del_element(Value, V) end,
  update_kv(Map, Namespace, Key, Fun).

remove_from_kv_dict(Map, Namespace, Key, DictKey) ->
  Fun = fun(V) -> maps:remove(DictKey, V) end,
  update_kv(Map, Namespace, Key, Fun).

add_into_kv_dict_dict(Map, Namespace, Key, Dict1Key, Dict2Key, Value) ->
  Fun = fun(Dict1) ->
            NewDict2 = case maps:find(Dict1Key, Dict1) of
                         {ok, Dict2} ->
                           maps:put(Dict2Key, Value, Dict2);
                         error ->
                           maps:from_list([{Dict2Key, Value}])
                       end,
            maps:put(Dict1Key, NewDict2, Dict1)
        end,
  update_kv(Map, Namespace, Key, Fun).

add_into_kv_dict_dict_set(Map, Namespace, Key, Dict1Key, Dict2Key, SetValue) ->
  Fun = fun(Dict1) ->
            NewDict2 = case maps:find(Dict1Key, Dict1) of
                         {ok, Dict2} ->
                           NewSet = case maps:find(Dict2Key, Dict2) of
                                      {ok, Set} -> gb_sets:add_element(SetValue, Set);
                                      error -> gb_sets:from_list([SetValue])
                                    end,
                           maps:put(Dict2Key, NewSet, Dict2);
                         error ->
                           Set = gb_sets:from_list([SetValue]),
                           maps:from_list([{Dict2Key, Set}])
                       end,
            maps:put(Dict1Key, NewDict2, Dict1)
        end,
  update_kv(Map, Namespace, Key, Fun).

alias_matcher({F, A, _} = FAP) ->
  fun({Alias, Original}, _Meta, Acc) ->
      case Original of
        {Fun, Arity} when F == Fun andalso A == Arity ->
          [{Alias, FAP} | Acc];
        Fun when F == Fun ->
          [{Alias, FAP} | Acc];
        _ ->
          Acc
      end
  end.

match_aliases(FAPList, AliasDict) ->
  lists:foldl(fun(FAP, Acc) ->
                  Match = maps:fold(alias_matcher(FAP), [], AliasDict),
                  Match ++ Acc
              end,
              [],
              FAPList).

filter_aliases(ToFilter, Aliases) ->
  lists:foldl(fun(AliasFAP, Acc) ->
                  case orddict:is_key(AliasFAP, Aliases) of
                    true -> Acc;
                    false -> [AliasFAP | Acc]
                  end
              end,
              [],
              ToFilter).

add_aliases(Map, Namespace, Kind, AliasFAPList) ->
  lists:foldl(fun({Alias0, FAP0}, M) -> add_alias(M, Namespace, Kind, Alias0, FAP0) end,
              Map,
              AliasFAPList).

check_and_add_alias(Map, Namespace, Kind, FAP) ->
  Aliases = match_aliases([FAP], get_kv(Map, Namespace, 'aliases')),
  add_aliases(Map, Namespace, Kind, Aliases).

gen_aliases(Map, Namespace, Alias, Original, Meta) ->
  L = [{'defn', 'funs'}, {'defmacro', 'macros'}],
  AliasDict = maps:from_list([{{Alias, Original}, Meta}]),
  lists:foldl(fun ({Kind, Key}, M) ->
                  Aliases = match_aliases(get_kv_set(M, Namespace, Key), AliasDict),
                  add_aliases(M, Namespace, Kind, Aliases)
              end,
              Map,
              L).

add_alias(Map, Namespace, Kind, Alias, {_, A, P} = FAP) ->
  Map1 = add_alias(Map, Namespace, Kind, {Alias, FAP}),
  %% Check whether this FAP is exported.
  %% Export Alias when the original name is exported.
  case is_exported(Map1, Namespace, Kind, FAP) of
    true ->
      add_export(Map1, Namespace, Kind, {Alias, A, P});
    false ->
      Map1
  end.

alias_exist(Map, Namespace, Alias) ->
  NameMatch = fun({A, _FAP}) -> A == Alias end,
  lists:any(NameMatch, get_kv_set(Map, Namespace, 'fun_aliases')) orelse
    lists:any(NameMatch, get_kv_set(Map, Namespace, 'macro_aliases')).

validate_aliases(Map, Namespace) ->
  Invalid = maps:fold(fun({Alias, _Original} = AliasOriginal, Meta, Acc) ->
                          case alias_exist(Map, Namespace, Alias) of
                            true -> Acc;
                            false -> [{AliasOriginal, Meta} | Acc]
                          end
                      end,
                      [],
                      get_kv(Map, Namespace, 'aliases')),
  case Invalid of
    [] -> ok;
    _ -> {invalid_alias, Invalid}
  end.

find_unresolved_fa(Suspended, Dependencies) ->
  [UnresolvedFA | _] = maps:keys(Suspended),
  find_unresolved_fa(Suspended, Dependencies, UnresolvedFA).

find_unresolved_fa(Suspended, Dependencies, UnresolvedFA) ->
  case kapok_dispatch:filter_fa(UnresolvedFA, maps:keys(Dependencies)) of
    [] ->
      UnresolvedDict = maps:get(UnresolvedFA, Suspended),
      [{_Order, {FAMeta, _Args}} | _] = maps:to_list(UnresolvedDict),
      {UnresolvedFA, FAMeta};
    [FAP | _] ->
      FA = maps:get(FAP, Dependencies),
      find_unresolved_fa(Suspended, Dependencies, FA)
  end.

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
  File = ?m(Ctx, file),
  Line = 1,
  AttrModule = {attribute, Line, module, ModuleName},
  AttrFile = {attribute, Line, file, {kapok_utils:characters_to_list(File), Line}},
  AttrExport = {attribute, Line, export, Exports},
  %% add other forms such as attributes
  OtherForms = get_kv_set(Map, Namespace, 'forms'),
  %% The `module_info' functions definitions and exports are added automatically
  %% by the erlang compiler, so it's not necessary to manually add them
  %% in kapok compiler.
  Forms = [AttrModule, AttrFile, AttrExport] ++ OtherForms ++ DefForms,
  {Forms, Ctx1}.

namespace_exports(NS, Options) ->
  GetExport = fun({F, A, _P}, Acc) -> gb_sets:add_element({F, A}, Acc) end,
  Functions = gb_sets:fold(GetExport, gb_sets:new(), maps:get('export_funs', NS)),
  Key = case lists:member(export_all_macro, Options) of
          true -> 'macros';
          false -> 'export_macros'
        end,
  Macros = gb_sets:fold(GetExport, gb_sets:new(), maps:get(Key, NS)),
  gb_sets:to_list(gb_sets:union(Functions, Macros)).

private_macros(NS) ->
  Exports = maps:get('export_macros', NS),
  All = maps:get('macros', NS),
  gb_sets:subtract(All, Exports).

namespace_defs(NS, Options) ->
  GetMetaClauses = fun({_Order, Meta}, Clauses, {nil, Acc}) ->
                       {Meta, gb_sets:to_list(Clauses) ++ Acc};
                      (_OrderAndMeta, Clauses, {Meta, Acc}) ->
                       {Meta, gb_sets:to_list(Clauses) ++ Acc}
                   end,
  IterateDefs = fun(FA, MetaClauses, Acc) ->
                    {Meta, ReversedClauses} = maps:fold(GetMetaClauses, {nil, []}, MetaClauses),
                    Line = ?line(Meta),
                    Clauses = lists:reverse(ReversedClauses),
                    orddict:store(FA, {Line, Clauses}, Acc)
                end,
  AllDefs = maps:fold(IterateDefs, orddict:new(), maps:get('defs', NS)),
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
                              gb_sets:is_empty(gb_sets:filter(Match, PrivateMacros))
                          end,
                 orddict:filter(Filter, AllDefs)
             end;
           false ->
             AllDefs
         end,
  AliasDefs = gen_alias_defs(NS, Options, Defs),
  %% append aliases defs into head since it's much shorter in general.
  orddict:merge(fun(_K, V1, _V2) -> V1 end, Defs, AliasDefs).


%% generate the aliases definitions.
gen_alias_defs(NS, _Options, Defs) ->
  %% insert aliases defs
  Aliases = gb_sets:union(maps:get('fun_aliases', NS), maps:get('macro_aliases', NS)),
  gb_sets:fold(fun({Alias, {F, A, _}}, Acc) ->
                   case orddict:find({F, A}, Defs) of
                     {ok, MetaClauses} -> orddict:store({Alias, A}, MetaClauses, Acc);
                     error -> Acc
                   end
               end,
               orddict:new(),
               Aliases).

%% generate the `__info__' function for kapok.
gen_info_fun(Namespace, Ctx, Options, Map) ->
  NS = maps:get(Namespace, Map),
  %% The `module_info' functions definitions and exports are automatically added by
  %% the erlang compiler, their exports need to be added to the kapok exported functions list.
  ModuleInfoFunctions = gb_sets:from_list([{'module_info', 0, 'normal'},
                                           {'module_info', 1, 'normal'}]),
  Functions = gb_sets:to_list(gb_sets:union(ModuleInfoFunctions, maps:get(export_funs, NS))),
  Macros = case lists:member(export_all_macro, Options) of
             true -> gb_sets:to_list(maps:get(macros, NS));
             false -> gb_sets:to_list(maps:get(export_macros, NS))
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
