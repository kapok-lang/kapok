%% symbol table for compilation
-module(kapok_symbol_table).
-behaviour(gen_server).
-export([add_namespace/1,
         add_fap/5,
         remove_fap/5,
         add_def/7,
         handle_aliases/2,
         add_suspended_alias/4,
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

handle_aliases(Namespace, Options) ->
  gen_server:call(?MODULE, {handle_aliases, Namespace, Options}).

add_suspended_alias(Namespace, Alias, Original, Meta) ->
  gen_server:call(?MODULE, {add_suspended_alias, Namespace, Alias, Original, Meta}).

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
  case gen_server:call(?MODULE, {namespace_forms, Namespace, ModuleName, Ctx, Options}) of
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
handle_call({add_ns, Namespace}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  {reply, ok, Map1};
%% fap
handle_call({add_fap, Namespace, Kind, Fun, Arity, ParameterType}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  FAP = {Fun, Arity, ParameterType},
  Map2 = add_def(Map1, Namespace, Kind, FAP),
  {reply, ok, Map2};
handle_call({remove_fap, Namespace, Kind, Fun, Arity, ParameterType}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  FAP = {Fun, Arity, ParameterType},
  Map2 = remove_def(Map1, Namespace, Kind, FAP),
  {reply, ok, Map2};

%% def
handle_call({add_def, Namespace, Kind, Fun, Arity, ParameterType, Meta, Clause}, _From, Map) ->
  %% TODO check duplication and conflict for defs
  Map1 = add_namespace_if_missing(Map, Namespace),
  FAP = {Fun, Arity, ParameterType},
  Map2 = add_def(Map1, Namespace, Kind, FAP),
  Map3 = add_export(Map2, Namespace, Kind, FAP),
  FA = {Fun, Arity},
  Map4 = add_def_clause(Map3, Namespace, Kind, FA, Meta, Clause),
  {reply, ok, Map4};
%% alias
handle_call({handle_aliases, Namespace, _Options}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  SuspendedAliases = get_kv(Map, Namespace, 'suspended_aliases'),
  Iterate = fun({Alias, Original}, _Meta, {Acc, M}) ->
                case gen_aliases(M, Namespace, 'funs', Alias, Original) of
                  {[], _, M1} ->
                    case gen_aliases(M1, Namespace, 'macros', Alias, Original) of
                      {[], _, M2}->
                        {Acc, M2};
                      {Match, Kind, M2} ->
                        {[{Kind, Match} | Acc], M2}
                    end;
                  {Match, Kind, M1} ->
                    {[{Kind, Match} | Acc], M1}
                end
            end,
  {Added, Map2} = orddict:fold(Iterate, {[], Map1}, SuspendedAliases),
  %% delete matched aliases from suspended aliases
  R = case get_kv(Map2, Namespace, 'suspended_aliases') of
        [] ->
          {ok, Added};
        InvalidAliases ->
          {error, orddict:to_list(InvalidAliases)}
      end,
  {reply, R, Map2};

handle_call({add_suspended_alias, Namespace, Alias, Original, Meta}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  %% TODO check duplication and conflict among aliases and defs
  %% TODO check duplication for AliasKey
  Map2 = add_into_kv_dict(Map1, Namespace, 'suspended_aliases', {Alias, Original}, Meta),
  {reply, ok, Map2};

%% form
handle_call({add_form, Namespace, Form}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  Map2 = add_into_kv_set(Map1, Namespace, 'forms', Form),
  {reply, ok, Map2};

%% pending def clause
handle_call({add_suspended_def_clause, Namespace, FA, ClauseArgs}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  Fun = fun(Clauses) ->
            Args = case orddict:find(FA, Clauses) of
                     {ok, V} -> ordsets:add_element(ClauseArgs, V);
                     error -> ordsets:from_list([ClauseArgs])
                   end,
            orddict:store(FA, Args, Clauses)
        end,
  Map2 = update_kv(Map1, Namespace, 'suspended_def_clauses', Fun),
  {reply, ok, Map2};

handle_call({remove_suspended_def_clause, Namespace, FA}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  Fun = fun(Clauses) -> orddict:erase(FA, Clauses) end,
  Map2 = update_kv(Map1, Namespace, 'suspended_def_clauses', Fun),
  {reply, ok, Map2};

handle_call({namespace_suspended_def_clause, Namespace}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  Suspended = get_kv(Map1, Namespace, 'suspended_def_clauses'),
  {reply, Suspended, Map1};

%% local functions and macros
handle_call({namespace_locals, Namespace}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  Aliases = get_kv(Map1, Namespace, 'aliases'),
  Macros = get_kv(Map1, Namespace, 'macros'),
  Funs = get_kv(Map1, Namespace, 'funs'),
  {reply, ordsets:union([Aliases, Macros, Funs]), Map1};
handle_call({namespace_funs, Namespace}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  FAList = get_kv(Map1, Namespace, 'funs'),
  {reply, FAList, Map1};
handle_call({namespace_macros, Namespace}, _From, Map) ->
  Map1 = add_namespace_if_missing(Map, Namespace),
  FAList = get_kv(Map1, Namespace, 'macros'),
  {reply, FAList, Map1};
handle_call({namespace_forms, Namespace, ModuleName, Ctx, Options}, _From, Map) ->
  R = case lists:member(ignore_suspended_def_clauses, Options) of
        false ->
          case get_kv(Map, Namespace, 'suspended_def_clauses') of
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
    suspended_aliases => [],
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
  Fun = fun(Defs) ->
            Clauses1 = case orddict:find(FA, Defs) of
                         {ok, Clauses} -> orddict:store(Meta, Clause, Clauses);
                         error -> orddict:from_list([{Meta, Clause}])
                       end,
            orddict:store(FA, Clauses1, Defs)
        end,
  update_kv(Map, Namespace, 'defs', Fun).

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

%% TODO The impl is similar with kapok_dispatch:filter_exports_by('rename').
gen_aliases(Map, Namespace, Key, Alias, Original) ->
  Kind = case Key of
           'funs' -> 'defn';
           'macros' -> 'defmacro'
         end,
  Match = fun({F, A, P} = FAP, {Acc, M}) ->
              case Original of
                {Fun, Arity} ->
                  if F == Fun andalso A == Arity ->
                      M1 = solve_alias(M, Namespace, Kind, Alias, Original, FAP),
                      {[{Alias, A, P} | Acc], M1};
                     true ->
                      {Acc, M}
                  end;
                Fun ->
                  if F == Fun ->
                      M1 = solve_alias(M, Namespace, Kind, Alias, Original, FAP),
                      {[{Alias, A, P} | Acc], M1};
                     true ->
                      {Acc, M}
                  end
              end
          end,
  {Added, Map1} = lists:foldl(Match, {[], Map}, get_kv(Map, Namespace, Key)),
  {Added, Kind, Map1}.

solve_alias(Map, Namespace, Kind, Alias, Original, {_, A, P} = FAP) ->
  Map1 = add_into_kv_set(Map, Namespace, 'aliases', {Alias, FAP}),
  Map2 = remove_from_kv_dict(Map1, Namespace, 'suspended_aliases', {Alias, Original}),
  %% Check whether this FAP is exported.
  %% Export Alias when the original name is exported.
  case is_exported(Map2, Namespace, Kind, FAP) of
    true ->
      add_export(Map2, Namespace, Kind, {Alias, A, P});
    false ->
      Map
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
  AliasDefs = gen_alias_defs(NS, Options, Defs),
  %% append aliases defs into head since it's much shorter in general.
  orddict:merge(fun(_K, V1, _V2) -> V1 end, Defs, AliasDefs).


%% generate the aliases definitions.
gen_alias_defs(NS, _Options, Defs) ->
  %% insert aliases defs
  Aliases = maps:get('aliases', NS),
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
