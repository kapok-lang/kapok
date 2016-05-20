%% A bunch of helpers to help to deal with errors in Kapok source code.
%% This is not exposed in the Kapok language.
-module(kapok_error).
-export([form_error/4,
         compile_error/3,
         compile_error/4,
         parse_error/4,
         handle_file_error/2
         ]).
-include("kapok.hrl").

%% General form error.

form_error(Meta, File, Module, ErrorDesc) ->
  compile_error(Meta, File, format_error(Module, ErrorDesc)).

%% Compilation error.

compile_error(Meta, File, Message) when is_list(Message) ->
  raise(Meta, File, 'CompileError', kapok_utils:characters_to_binary(Message)).

compile_error(Meta, File, Format, Args) when is_list(Format) ->
  compile_error(Meta, File, io_lib:format(Format, Args)).

%% Tokenization/parsing error.

parse_error(Line, File, Module, ErrorDesc) ->
  Message = format_error(Module, ErrorDesc),
  raise(Line, File, 'SyntaxError', kapok_utils:characters_to_binary(Message)).

%% Handle warnings and errors from Erlang land (called during module compilation)

handle_file_error(File, {Line, erl_lint, {unsafe_to_atom, Var, {In, _Where}}}) ->
  Translated = case In of
    'orelse'  -> 'or';
    'andalso' -> 'and';
    _ -> In
  end,
  Message = io_lib:format("cannot define variable ~ts inside ~ts", [Var, Translated]),
  raise(Line, File, 'CompileError', kapok_utils:characters_to_binary(Message)).

%% Helpers

raise(Meta, File, Kind, Message) when is_list(Meta), is_binary(File), is_binary(Message)  ->
  Line = ?line(Meta),
  raise(Line, File, Kind, Message);
raise(Line, File, Kind, Message) when is_integer(Line), is_binary(File), is_binary(Message) ->
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

format_error(Module, ErrorDesc) ->
  Module:format_error(ErrorDesc).

