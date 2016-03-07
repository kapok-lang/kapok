%% Main entry point for Kapok functions.
-module(kapok).
-behaviour(application).
-export([start_cli/0]).
-include("kapok.hrl").

%% OTP Application API

-export([start/2, stop/1]).

start(_Type, _Args) ->
  case kapok_sup:start_link() of
    {ok, Sup} ->
      {ok, Sup};
    {error, _Reason} = Error ->
      Error
  end.

stop(_State) ->
  ok.

%% Boot and process given options. Invoked by Kapok's script.

start_cli() ->
  {ok, _} = application:ensure_all_started(?MODULE),

  kapok_cli:main(init:get_plain_arguments()).

