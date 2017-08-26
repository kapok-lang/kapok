%% Helper functions used throughout kapok source code.

-module(kapok_utils).
-export([get_line/1,
         characters_to_list/1,
         characters_to_binary/1,
         to_binary/1,
         read_file_type/1,
         relative_to_cwd/1,
         gensym_plain/1,
         gensym/0,
         gensym/1
        ]).
-include_lib("kernel/include/file.hrl").

get_line(Opts) when is_list(Opts) ->
  case lists:keyfind(line, 1, Opts) of
    {line, Line} when is_integer(Line) -> Line;
    false -> 0
  end.

characters_to_list(List) when is_list(List) ->
  List;
characters_to_list(Data) ->
  unicode:characters_to_list(Data).

characters_to_binary(Bin) when is_binary(Bin) ->
  Bin;
characters_to_binary(List) when is_list(List) ->
  case unicode:characters_to_binary(List) of
    Result when is_binary(Result) ->
      Result;
    {error, Encoded, Rest} ->
      throw({invalid, Encoded, Rest});
    {incomplete, Encoded, Rest} ->
      throw({incomplete, Encoded, Rest})
  end.

to_binary(List) when is_list(List) -> unicode:characters_to_binary(List);
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8).

read_file_type(File) ->
  case file:read_file_info(File) of
    {ok, #file_info{type = Type}} -> {ok, Type};
    {error, _} = Error -> Error
  end.

relative_to_cwd(Path) ->
  %% TODO add external path library call
  Path.

gensym_plain(Prefix) ->
  gensym("~s~s", [Prefix, gensym_random()]).

gensym() ->
  gensym("#:G~s", [gensym_random()]).

gensym(Prefix) ->
  gensym("#:|~s~s|", [Prefix, gensym_random()]).

gensym(Format, Args) ->
  erlang:list_to_atom(lists:flatten(io_lib:format(Format, Args))).

gensym_random() ->
  X = erlang:phash2({erlang:node(), erlang:timestamp(), crypto:strong_rand_bytes(16)}),
  lists:map(fun(E) -> io_lib:format("~.16B", [E]) end,
            erlang:binary_to_list(binary:encode_unsigned(X))).
