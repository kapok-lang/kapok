%% A bunch of helpers to help to deal with errors in Ceiba source code.
%% This is not exposed in the Ceiba language.
-module(ceiba_error).
-export([parse_error/4,
         compile_error/3,
         compile_error/4]).
-include("ceiba.hrl").

%% Tokenization/parsing error.

parse_error(Line, File, Module, ErrorDesc) ->
  Message = Module:format_error(ErrorDesc),
  do_raise(Line, File, 'syntax-error', Message).

%% Compilation error.

compile_error(Meta, File, Message) when is_list(Message) ->
  raise(Meta, File, 'compile-error', Message).

compile_error(Meta, File, Format, Args) when is_list(Format) ->
  compile_error(Meta, File, io_lib:format(Format, Args)).

%% Helpers

raise(Meta, File, Kind, Message) when is_list(Meta) ->
  {MetaLine, MetaFile} = meta_location(Meta, File),
  do_raise(MetaLine, MetaFile, Kind, Message).

do_raise({Line, _}, File, Kind, Message) when is_integer(Line) ->
  do_raise(Line, File, Kind, Message);
do_raise(Line, File, Kind, Message)
    when is_binary(File), is_integer(Line), is_binary(Message) ->
  %% reset stacktrace
  try
    throw(ok)
  catch
    ok -> ok
  end,
  Stacktrace = erlang:get_stacktrace(),
  Exception = Kind(File, Line, Message),
  erlang:raise(error, Exception, tl(Stacktrace)).

meta_location(Meta, File) ->
  case lists:keyfind(file, 1, Meta) of
    {file, MetaFile} when is_binary(MetaFile) ->
      case lists:keyfind(keep, 1, Meta) of
        {keep, MetaLine} when is_integer(MetaLine) -> ok;
        _ -> MetaLine = 0
      end,
      {MetaLine, MetaFile};
    _ ->
      {?line(Meta), File}
  end.

%% error kind functions

'syntax-error'(File, Line, Message) ->
  {'syntax-error', File, Line, Message}.

'compile-error'(File, Line, Message) ->
  {'compile-error', File, Line, Message}.
