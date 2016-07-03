%% namespace
-module(kapok_namespace).
-export([init_namespace_table/0,
         add_namespace/1,
         add_clause/5,
         namespace_defs/1,
         namespace_export_functions/1,
         namespace_export_macros/1,
         add_export/5,
         namespace_exports/1,
         module_exports/2,
         namespace_aliases/2,
         add_alias/3,
         namespace_locals/1,
         get_local/2,
         add_local/4,
         namespace_forms/1,
         add_form/2,
         info_fun/3]).
-include("kapok.hrl").

init_namespace_table() ->
  _ = ets:new(kapok_namespaces, [set, protected, named_table, {read_concurrency, true}]).

add_namespace(Namespace) ->
  ets:insert(kapok_namespaces, {Namespace, [], [], [], [], [], [], []}).

add_clause(Namespace, Kind, Fun, Arity, Clause) when Kind == 'defn' ->
  add_clause(Namespace, 2, Fun, Arity, Clause);
add_clause(Namespace, Kind, Fun, Arity, Clause) when Kind == 'defn-' ->
  add_clause(Namespace, 2, Fun, Arity, Clause);
add_clause(Namespace, Kind, Fun, Arity, Clause) when Kind == 'defmacro' ->
  add_clause(Namespace, 3, Fun, Arity, Clause);
add_clause(Namespace, Index, Fun, Arity, Clause) when is_integer(Index) ->
  Old = ets:lookup_element(kapok_namespaces, Namespace, Index),
  Key = {Fun, Arity},
  C = case orddict:find(Key, Old) of
        {ok, Clauses} -> [Clause|Clauses];
        error -> [Clause]
      end,
  New = orddict:store(Key, C, Old),
  ets:update_element(kapok_namespaces, Namespace, {Index, New}).

namespace_functions(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 2).

namespace_macros(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 3).

namespace_defs(Namespace) ->
  Functions = namespace_functions(Namespace),
  Macros = namespace_macros(Namespace),
  %% TODO macro will shadow function if there is any overrides
  orddict:merge(fun(_K, _V1, V2) -> V2 end, Functions, Macros).

namespace_export_functions(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 4).

namespace_export_macros(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 5).

add_export(_Namespace, Kind, _F, _A, _ParameterType) when Kind == 'defn-'; Kind == 'defalias-' ->
  %% don't export for namespace private function definitions
  ok;
add_export(Namespace, Kind, F, A, ParameterType) when Kind == 'defn'; Kind == 'defalias' ->
  add_export(Namespace, 4, F, A, ParameterType);
add_export(Namespace, Kind, F, A, ParameterType) when Kind == 'defmacro' ->
  add_export(Namespace, 5, F, A, ParameterType);
add_export(Namespace, Index, F, A, ParameterType) when is_integer(Index) ->
  OldExports = ets:lookup_element(kapok_namespaces, Namespace, Index),
  NewExports = ordsets:add_element({F, A, ParameterType}, OldExports),
  ets:update_element(kapok_namespaces, Namespace, {Index, NewExports}).

namespace_exports(Namespace) ->
  Functions = namespace_export_functions(Namespace),
  Macros = namespace_export_macros(Namespace),
  ordsets:union(Functions, Macros).

module_exports(Functions, Macros) ->
  GetExport = fun({F, A, _P}) -> {F, A} end,
  ExportFunctions = ordsets:from_list(lists:map(GetExport, Functions)),
  ExportMacros = ordsets:from_list(lists:map(GetExport, Macros)),
  %% TODO macro will shadow function if there is any overrides
  ordsets:union(ExportFunctions, ExportMacros).

namespace_aliases(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 6).

namespace_aliases(Namespace, Defs) ->
  Aliases = namespace_aliases(Namespace),
  lists:foldl(fun({Alias, F, A}, Acc) ->
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
              Aliases).

add_alias(Namespace, Alias, Original) ->
  Aliases = namespace_aliases(Namespace),
  NewAliases = case Original of
                 {Fun, Arity} -> ordsets:add_element({Alias, Fun, Arity}, Aliases);
                 Fun -> ordsets:add_element({Alias, Fun}, Aliases)
               end,
  ets:update_element(kapok_namespaces, Namespace, {6, NewAliases}).

namespace_locals(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 7).

get_local(Namespace, {Fun, Arity}) ->
  ordsets:filter(fun({F, A, _P}) -> (F == Fun) andalso (A == Arity) end,
                 namespace_locals(Namespace));
get_local(Namespace, Fun) ->
  ordsets:filter(fun({F, _A, _P}) -> F == Fun end,
                 namespace_locals(Namespace)).

add_local(Namespace, Fun, Arity, ParameterType) ->
  Locals = namespace_locals(Namespace),
  NewLocals = ordsets:add_element({Fun, Arity, ParameterType}, Locals),
  ets:update_element(kapok_namespaces, Namespace, {7, NewLocals}).

namespace_forms(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 8).

add_form(Namespace, Form) ->
  Forms = namespace_forms(Namespace),
  NewForms = ordsets:add_element(Form, Forms),
  ets:update_element(kapok_namespaces, Namespace, {8, NewForms}).

info_fun(Functions, Macros, Env) ->
  {FunctionList, _} = kapok_translate:translate(kapok_expand:quote(Functions), Env),
  {MacroList, _} = kapok_translate:translate(kapok_expand:quote(Macros), Env),
  {function, 0,'__info__',1,
   [{clause,0,[{atom,0,'functions'}],[],[FunctionList]},
    {clause,0,[{atom,0,'macros'}],[],[MacroList]}]}.


