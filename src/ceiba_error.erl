%% A bunch of helpers to help to deal with errors in Ceiba source code.
%% This is not exposed in the Ceiba language.
-module(ceiba_error).
-export([parse_error/4]).
-include("ceiba.hrl").

%% Tokenization/parsing errors.


parse_error(Location, File, Module, ErrorDesc) ->
    Message = Module:format_error(ErrorDesc),
    do_raise(Location, File, 'syntax-error', Message).

do_raise(File, {Line, _}, Kind, Message) when is_integer(Line) ->
    do_raise(File, Line, Kind, Message);
do_raise(File, Line, Kind, Message)
  when is_integer(Line), is_binary(File), is_binary(Message) ->
    %% reset stacktrace
    try
        throw(ok)
    catch
        ok -> ok
    end,
    Stacktrace = erlang:get_stacktrace(),
    Exception = Kind(File, Line, Message),
    erlang:raise(error, Exception, tl(Stacktrace)).



'syntax-error'(File, Line, Desc) ->
    {'syntax-error', File, Line, Desc}.
