%% An Erlang module that behaves like an Kapok module used for bootstraping.
-module(kapok_bootstrap).
-export(['MACRO-defn'/2,
         'MACRO-defn-'/2,
         'MACRO-defmacro'/2,
         '__info__'/1]).

-define(identifier(Id), {identifier, [], Id}).

'MACRO-defn'(Args, Env) ->
  define('defn', Args, Env).

'MACRO-defn-'(Args, Env) ->
  define('defn-', Args, Env).

'MACRO-defmacro'(Args, Env) ->
  define('defmacro', Args, Env).


'__info__'(functions) ->
  [];
'__info__'(macros) ->
  [{'MACRO-defn', 2},
   {'MACRO-defn-', 2},
   {'MACRO-defmacro', 2}].

define(F, Args, Env) ->
  QArgs = kapok_macro:quote(Args),
  QEnv = kapok_macro:quote(Env),
  {{{dot, [], {?identifier(kapok_def)}, ?identifier(F)}, QArgs}, QEnv}.
