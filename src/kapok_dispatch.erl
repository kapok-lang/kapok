%% Helpers related to dispatching to imports and references.
%% This module access the information stored on the scope
%% by kapok_import and therefore assumes it is normalized (ordsets)
-module(kapok_dispatch).
-export([default_functions/0, default_macros/0, default_requires/0]).
-include("kapok.hrl").


default_functions() ->
  [].

default_macros() ->
  [].

default_requires() ->
  [].

