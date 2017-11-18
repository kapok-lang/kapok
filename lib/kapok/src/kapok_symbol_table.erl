%% symbol table for compilation
-module(kapok_symbol_table).
-behaviour(gen_server).
-export([new_namespace/1,
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
-include("kapok.hrl").

%% Public API

new_namespace(Namespace) ->
  gen_server:call(?MODULE, {new_ns, Namespace}).

new_def(Namespace, Kind, Fun, Arity, ParameterType, Meta, Clause) ->
  gen_server:call(?MODULE, {new_def, Namespace, Kind, Fun, Arity, ParameterType, Meta, Clause}).

new_alias(Namespace, Alias, Original, Meta) ->
  gen_server:call(?MODULE, {new_alias, Namespace, Alias, Original, Meta}).

new_form(Namespace, Form) ->
  gen_server:call(?MODULE, {new_form, Namespace, Form}).

new_suspended_def_clause(Namespace, FA, ClauseArgs) ->
  gen_server:call(?MODULE, {new_suspended_def_clause, Namespace, FA, ClauseArgs}).

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
    {suspended, [SuspendedDef | _T], Ctx1} ->
      {FA, [{FAMeta, _} | _]} = SuspendedDef,
      kapok_error:compile_error(FAMeta, ?m(Ctx1, file), "unknown local call: ~p", [FA]);
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
handle_call({new_suspended_def_clause, Namespace, FA, ClauseArgs}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  Map2 = add_suspended_def_clause(Map1, Namespace, FA, ClauseArgs),
  {reply, ok, Map2};

handle_call({check_suspended_def_clauses, Namespace, Kind, FAPList}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  Match = match_aliases(FAPList, get_kv(Map1, Namespace, 'aliases')),
  Key = case Kind of
          K1 when K1 == 'defn'; K1 == 'defn-' -> 'fun_aliases';
          K2 when K2 == 'defmacro'; K2 == 'defmacro-' -> 'macro_aliases'
        end,
  NewAliasFAPList = filter_aliases(Match, get_kv(Map1, Namespace, Key)),
  AllFAPList = NewAliasFAPList ++ FAPList,
  SuspendedClauses = get_kv(Map1, Namespace, 'suspended_def_clauses'),
  Folder = fun(FA, S, {Suspended, M} = Acc) ->
               case kapok_dispatch:filter_fa(FA, AllFAPList) of
                 [] ->
                   Acc;
                 _ ->
                   M1 = remove_suspended_def_clause(M, Namespace, FA),
                   {orddict:store(FA, S, Suspended), M1}
               end
           end,
  {ToHandle, Map2} = orddict:fold(Folder,
                                  {orddict:new(), Map1},
                                  SuspendedClauses),
  Map4 = case ToHandle of
           [] ->
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
  Map1 = add_namespace_if_missing(Map, Namespace),
  %% direct macro definitions
  Macros = get_kv(Map1, Namespace, 'macros'),
  Aliases = get_kv(Map1, Namespace, 'macro_aliases'),
  {reply, Aliases ++ Macros, Map1};

handle_call({namespace_locals, Namespace}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  Funs = get_kv(Map1, Namespace, 'funs'),
  FunAliases = get_kv(Map1, Namespace, 'fun_aliases'),
  Macros = get_kv(Map1, Namespace, 'macros'),
  MacroAliases = get_kv(Map1, Namespace, 'macro_aliases'),
  {reply, ordsets:union([Funs, Macros, FunAliases, MacroAliases]), Map1};

handle_call({namespace_forms, Namespace, ModuleName, Ctx, Options}, _From, Map) ->
  R = case lists:member(ignore_suspended_def_clauses, Options) of
        false ->
          case get_kv(Map, Namespace, 'suspended_def_clauses') of
            [] ->
              case validate_aliases(Map, Namespace) of
                {invalid_alias, _} = V ->
                  V;
                ok ->
                  {Forms, Ctx1} = namespace_forms(Namespace, ModuleName, Ctx, Options, Map),
                  {ok, Forms, Ctx1}
              end;
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
    fun_aliases => [],
    macro_aliases => [],
    defs => [],
    locals => [],
    forms => [],
    suspended_def_clauses => []}.

add_namespace_if_missing(Map, Namespace) ->
  case maps:is_key(Namespace, Map) of
    false -> maps:put(Namespace, new_namespace(), Map);
    true -> Map
  end.

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
  ordsets:is_element(FAP, maps:get(Key, NS)).

add_def_clause(Map, Namespace, Kind, FA, Meta, Clause) when Kind == 'defn-' ->
  add_def_clause(Map, Namespace, 'defn', FA, Meta, Clause);
add_def_clause(Map, Namespace, Kind, FA, Meta, Clause) when Kind == 'defmacro-' ->
  add_def_clause(Map, Namespace, 'defmacro', FA, Meta, Clause);
add_def_clause(Map, Namespace, Kind, FA, Meta, Clause) when Kind == 'defn'; Kind == 'defmacro' ->
  add_into_kv_dict_dict_set(Map, Namespace, 'defs', FA, Meta, Clause).

add_suspended_def_clause(Map, Namespace, FA, ClauseArgs) ->
  add_into_kv_dict_set(Map, Namespace, 'suspended_def_clauses', FA, ClauseArgs).

remove_suspended_def_clause(Map, Namespace, FA) ->
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

add_into_kv_set(Map, Namespace, Key, List) when is_list(List) ->
  Fun = fun(V) -> ordsets:union([ordsets:from_list(List), V]) end,
  update_kv(Map, Namespace, Key, Fun);
add_into_kv_set(Map, Namespace, Key, Value) ->
  Fun = fun(V) -> ordsets:add_element(Value, V) end,
  update_kv(Map, Namespace, Key, Fun).

add_into_kv_dict(Map, Namespace, Key, DictKey, DictValue) ->
  Fun = fun(V) -> orddict:store(DictKey, DictValue, V) end,
  update_kv(Map, Namespace, Key, Fun).

remove_from_kv_set(Map, Namespace, Key, List) when is_list(List) ->
  Fun = fun(V) -> ordsets:subtract(V, ordsets:from_list(List)) end,
  update_kv(Map, Namespace, Key, Fun);
remove_from_kv_set(Map, Namespace, Key, Value) ->
  Fun = fun(V) -> ordsets:del_element(Value, V) end,
  update_kv(Map, Namespace, Key, Fun).

remove_from_kv_dict(Map, Namespace, Key, DictKey) ->
  Fun = fun(V) -> orddict:erase(DictKey, V) end,
  update_kv(Map, Namespace, Key, Fun).

add_into_kv_dict_set(Map, Namespace, Key, DictKey, SetValue) ->
  Fun = fun(Dict) ->
            NewSet = case orddict:find(DictKey, Dict) of
                       {ok, Set} -> ordsets:add_element(SetValue, Set);
                       error -> ordsets:from_list([SetValue])
                     end,
            orddict:store(DictKey, NewSet, Dict)
        end,
  update_kv(Map, Namespace, Key, Fun).

add_into_kv_dict_dict_set(Map, Namespace, Key, Dict1Key, Dict2Key, SetValue) ->
  Fun = fun(Dict1) ->
            NewDict2 = case orddict:find(Dict1Key, Dict1) of
                         {ok, Dict2} ->
                           NewSet = case orddict:find(Dict2Key, Dict2) of
                                      {ok, Set} -> ordsets:add_element(SetValue, Set);
                                      error -> ordsets:from_list([SetValue])
                                    end,
                           orddict:store(Dict2Key, NewSet, Dict2);
                         error ->
                           Set = ordsets:from_list([SetValue]),
                           orddict:from_list([{Dict2Key, Set}])
                       end,
            orddict:store(Dict1Key, NewDict2, Dict1)
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
                  Match = orddict:fold(alias_matcher(FAP), [], AliasDict),
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
  lists:foldl(fun ({Kind, Key}, M) ->
                  Aliases = match_aliases(get_kv(M, Namespace, Key), [{{Alias, Original}, Meta}]),
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
  lists:any(NameMatch, get_kv(Map, Namespace, 'fun_aliases')) orelse
    lists:any(NameMatch, get_kv(Map, Namespace, 'macro_aliases')).

validate_aliases(Map, Namespace) ->
  Invalid = orddict:fold(fun({Alias, _Original} = AliasOriginal, Meta, Acc) ->
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
  OtherForms = get_kv(Map, Namespace, 'forms'),
  %% The `module_info' functions definitions and exports are added automatically
  %% by the erlang compiler, so it's not necessary to manually add them
  %% in kapok compiler.
  Forms = [AttrModule, AttrFile, AttrExport] ++ OtherForms ++ DefForms,
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
  GetMetaClauses = fun(Meta, Clauses, {nil, Acc}) -> {Meta, Clauses ++ Acc};
                      (_Meta, Clauses, {Meta, Acc}) -> {Meta, Clauses ++ Acc}
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
  AliasDefs = gen_alias_defs(NS, Options, Defs),
  %% append aliases defs into head since it's much shorter in general.
  orddict:merge(fun(_K, V1, _V2) -> V1 end, Defs, AliasDefs).


%% generate the aliases definitions.
gen_alias_defs(NS, _Options, Defs) ->
  %% insert aliases defs
  Aliases = maps:get('fun_aliases', NS) ++ maps:get('macro_aliases', NS),
  lists:foldl(fun({Alias, {F, A, _}}, Acc) ->
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
