%% An Erlang module that behaves like an Kapok module used for bootstraping.
-module(kapok_bootstrap).
-export(['MACRO-ns'/2,
         'MACRO-defn'/2,
         'MACRO-defn-'/2,
         'MACRO-defmacro'/2,
         '__info__'/1]).

-define(identifier(Id), {identifier, [], Id}).

'MACRO-ns'({_Line, Env}, Args) ->
  %% expand ns macro here.
  io:format("craft!!!!"),
  {Args, Env}.

'MACRO-defn'(Caller, Args) ->
  define(Caller, 'defn', Args).

'MACRO-defn-'(Caller, Args) ->
  define(Caller, 'defn-', Args).

'MACRO-defmacro'(Caller, Args) ->
  define(Caller, 'defmacro', Args).


'__info__'(functions) ->
  [];
'__info__'(macros) ->
  [{'MACRO-ns', 2},
   {'MACRO-defn', 2},
   {'MACRO-defn-', 2},
   {'MACRO-defmacro', 2}].

define({Line, Env}, Kind, Args) ->
  QuotedArgs = kapok_macro:quote(Args),
  QEnv = kapok_macro:quote(Env),
  Args = [Line, Kind, QuotedArgs, QEnv],
  {{{dot, [], {?identifier(kapok_def)}, ?identifier(store_definition)}, Args}, QEnv}.
