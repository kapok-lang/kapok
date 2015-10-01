%% Main entry point for Ceiba functions. All of those functions are
%% private to the Ceiba compiler and reserved to be used by Ceiba only.
-module(ceiba).
-behaviour(application).
-export([string_to_ast/4]).


%% Converts a given string (char list) into AST.

string_to_ast(String, StartLine, File, Options)
  when is_integer(StartLine), is_binary(File) ->
    case ceiba_scanner:scan(String, StartLine, [{file, File}|Options]) of
        {ok, _Line, _Column, Tokens} ->
            try ceiba_parser:parse(Tokens) of
                {ok, Forms} -> io:format("~p~n", [Forms]);
                {error, {Line, _, [Error, Token]}} ->
                    {error, {Line, to_binary(Error), to_binary(Token)}}
            catch
                {error, {Line, _, [Error, Token]}} ->
                    {error, {Line, to_binary(Error), to_binary(Token)}}
            end;
        {error, {Location, Module, ErrorDescription}, _Rest, _SoFar} ->
            {error, Location, Module:format_error(ErrorDescription)}
    end.

to_binary(List) when is_list(List) -> unicode:characters_to_binary(List);
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8).
