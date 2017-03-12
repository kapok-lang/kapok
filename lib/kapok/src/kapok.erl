%% Main entry point for Kapok functions.
-module(kapok).
-behaviour(application).
-export([start_cli/0]).
-include("kapok.hrl").

%% OTP Application API

-export([start/2, stop/1, config_change/3]).

start(_Type, _Args) ->
  %% In case there is a shell, we can't really change its
  %% encoding, so we just set binary to true. Otherwise
  %% we must set the encoding as the user with no shell
  %% has encoding set to latin1.
  Opts =
    case init:get_argument(noshell) of
      {ok, _} -> [binary, {encoding, utf8}];
      error   -> [binary]
    end,

  ok = io:setopts(standard_io, Opts),

  %% TODO: Remove this once we support only OTP >18
  ok = case io:setopts(standard_error, [{encoding, utf8}]) of
         ok         -> ok;
         {error, _} -> io:setopts(standard_error, [{unicode, true}]) %% OTP 17.3 and earlier
       end,

  case file:native_name_encoding() of
    latin1 ->
      io:format(standard_error,
                "warning: the VM is running with native name encoding of latin1 which may cause "
                "Kapok to malfunction as it expects utf8. Please ensure your locale is set to UTF-8"
                " (which can be verified by running \"locale\" in your shell)~n", []);
    _ ->
      ok
  end,

  URIs = [{<<"ftp">>, 21},
          {<<"sftp">>, 22},
          {<<"tftp">>, 69},
          {<<"http">>, 80},
          {<<"https">>, 443},
          {<<"ldap">>, 389}],
  URIConfig = [{{uri, Scheme}, Port} || {Scheme, Port} <- URIs],
  CompilerOpts = [{docs, true}, {debug_info, true}, {warnings_as_errors, false}],
  Config = [{at_exit, []},
            {compiler_options, orddict:from_list(CompilerOpts)}
            | URIConfig],
  Tid = kapok_config:new(Config),
  case kapok_sup:start_link() of
    {ok, Sup} ->
      {ok, Sup, Tid};
    {error, _Reason} = Error ->
      Error
  end.

stop(Tid) ->
  kapok_config:delete(Tid).

config_change(_Changed, _New, _Remove) ->
  ok.

%% Boot and process given options. Invoked by Kapok's script.

start_cli() ->
  {ok, _} = application:ensure_all_started(?MODULE),

  kapok_cli:main(init:get_plain_arguments()).
