%% A bunch of helpers to help to deal with errors in Kapok source code.
%% This is not exposed in the Kapok language.
-module(kapok_error).
-export([form_error/4,
         compile_error/3,
         compile_error/4,
         parse_error/4,
         warn/2,
         warn/3,
         handle_file_warning/2,
         handle_file_error/2
        ]).
-include("kapok.hrl").

warn(Line, File, Warning) when is_integer(Line), is_binary(File) ->
  warn(file_format(Line, File), Warning).

warn(Caller, Warning) ->
  io:put_chars(standard_error, [Caller, "warning: ", Warning, $\n]),
  ok.

%% General form error.

form_error(Meta, File, Module, ErrorDesc) ->
  compile_error(Meta, File, format_error(Module, ErrorDesc)).

%% Compilation error.

compile_error(Meta, File, Message) when is_list(Message) ->
  compile_error(Meta, File, kapok_utils:characters_to_binary(Message));
compile_error(Meta, File, Message) when is_binary(Message) ->
  raise(Meta, File, 'CompileError', Message).

compile_error(Meta, File, Format, Args) ->
  compile_error(Meta, File, io_lib:format(Format, Args)).

%% Tokenization/parsing error.

parse_error(Line, File, Module, ErrorDesc) ->
  Message = format_error(Module, ErrorDesc),
  raise(Line, File, 'SyntaxError', kapok_utils:characters_to_binary(Message)).

%% Handle warnings and errors from Erlang land (called during module compilation)

%% Default behaviour
handle_file_warning(File, {Line, Module, Desc}) ->
  Message = format_error(Module, Desc),
  warn(Line, File, Message).


handle_file_error(File, {Line, erl_lint, {unsafe_to_atom, Var, {In, _Where}}}) ->
  Translated = case In of
                 'orelse'  -> 'or';
                 'andalso' -> 'and';
                 _ -> In
               end,
  Message = io_lib:format("cannot define variable ~ts inside ~ts", [Var, Translated]),
  raise(Line, File, 'CompileError', kapok_utils:characters_to_binary(Message));

%% Default behaviour
handle_file_error(File, {Line, Module, Desc}) ->
  Message = format_error(Module, Desc),
  raise(Line, File, 'CompileError', kapok_utils:characters_to_binary(Message)).


%% Helpers

raise(Meta, File, Kind, Message) when is_list(Meta), is_binary(File), is_binary(Message)  ->
  Line = ?line(Meta),
  raise(Line, File, Kind, Message);
raise(Line, File, Kind, Message) when is_integer(Line), is_binary(File), is_binary(Message) ->
  io:format("~p, file: ~p, line: ~p, ~s\n\n", [Kind, File, Line, Message]),
  %% reset stacktrace
  Stacktrace = try
    throw(ok)
  catch
    ok -> erlang:get_stacktrace()
  end,
  Exception = {Kind, File, Line, Message},
  erlang:raise(error, Exception, tl(Stacktrace)).

file_format(0, File) ->
  io_lib:format("~ts: ", [kapok_utils:relative_to_cwd(File)]);
file_format(Line, File) ->
  io_lib:format("~ts:~w: ", [kapok_utils:relative_to_cwd(File), Line]).


format_error([], Desc) ->
  io_lib:format("~p", [Desc]);
format_error(Module, ErrorDesc) ->
  Module:format_error(ErrorDesc).
