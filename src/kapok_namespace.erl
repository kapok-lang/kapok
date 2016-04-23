%%
-module(kapok_namespace).
-export([translate/3,
         init_namespace_table/0,
         add_namespace/1,
         add_export/2,
         namespace_functions/1,
         namespace_macros/1,
         namespace_exports/1,
         export_forms/1
        ]).

-include("kapok.hrl").


%% helpers

export_forms(Namespace) ->
  Exports = namespace_exports(Namespace),
  {attribute,0,export,[sets:to_list(Exports)]}.


%% namespace table

init_namespace_table() ->
  _ = ets:new(kapok_namespaces, [set, protected, named_table, {read_concurrency, true}]).

add_namespace(Tuple) ->
  ets:insert(kapok_namespaces, Tuple).

namespace_functions(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 2).

namespace_macros(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 3).

add_export(Namespace, Export) ->
  OldExports = namespace_exports(Namespace),
  NewExports = sets:add_element(Export, OldExports),
  ets:update_element(kapok_namespaces, Namespace, {4, NewExports}).

namespace_exports(Namespace) ->
  ets:lookup_element(kapok_namespaces, Namespace, 4).

%% Translation

translate(Meta, [{identifier, _, "ns"}], Env) ->
  kapok_error:compile_error(Meta, maps:get(file, Env), "no namespace");

translate(Meta, [{identifier, _, "ns"}, {identifier, _, Id}|_T], Env) ->
  Name = list_to_atom(Id),
  %%add_namespace({Name, maps:new(), maps:new(), sets:new()}),
  Line = ?line(Meta),
  {{attribute, Line, module, Name}, Env}.

