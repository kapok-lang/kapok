%% A bunch of helpers to help to deal with errors in Kapok source code.
%% This is not exposed in the Kapok language.
-module(kapok_error).
-export([parse_error/4,
         compile_error/3,
         compile_error/4]).
-include("kapok.hrl").

%% Tokenization/parsing error.

parse_error(Line, File, Module, ErrorDesc) ->
  Message = Module:format_error(ErrorDesc),
  raise(Line, File, 'syntax-error', Message).

%% Compilation error.

compile_error(Meta, File, Message) when is_list(Message) ->
  raise(Meta, File, 'compile-error', Message).

compile_error(Meta, File, Format, Args) when is_list(Format) ->
  compile_error(Meta, File, io_lib:format(Format, Args)).

%% Helpers


raise(Meta, File, Kind, Message) when is_list(Meta), is_binary(File), is_list(Message)  ->
  Line = ?line(Meta),
  raise(Line, File, Kind, Message);
raise(Line, File, Kind, Message) when is_integer(Line), is_binary(File), is_list(Message) ->
  io:format("~p, file: ~p, line: ~p, ~s\n\n", [Kind, File, Line, Message]),
  %% reset stacktrace
  try
    throw(ok)
  catch
    ok -> ok
  end,
  Stacktrace = erlang:get_stacktrace(),
  Exception = {Kind, File, Line, Message},
  erlang:raise(error, Exception, tl(Stacktrace)).

