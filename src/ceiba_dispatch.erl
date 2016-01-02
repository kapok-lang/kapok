%% Helpers related to dispatching to imports and references.
%% This module access the information stored on the scope
%% by ceiba_import and therefore assumes it is normalized (ordsets)
-module(ceiba_dispatch).
-export([default_functions/0, default_macros/0, default_requires/0]).
-include("ceiba.hrl").


default_functions() ->
    [].

default_macros() ->
    [].

default_requires() ->
    [].

