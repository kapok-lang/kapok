%% Command line functionals.
-module(kapok_cli).
-export([main/1,
         run/1]).
-include("kapok.hrl").

%% config hold all infos parsed from command line options and arguments.
blank_config() ->
  #{commands => [],
    compile => [],
    compiler_options => [],
    outdir => ".",
    pa => [],
    pz => [],
    verbose_compile => false,
    halt => true,
    errors => []}.

%% Public API
main(Args) ->
  {Config, _} = parse_args(Args),
  run(fun(_) ->
          Errors = process_commands(Config),
          if
            Errors =/= [] ->
              lists:map(fun(E) -> io:format(standard_error, "~s~n", [E]) end,
                        Errors);
            true -> ok
          end
      end,
      ?m(Config, halt)).

%% run the specified fun on child process
run(Fun) ->
  run(Fun, true).
run(Fun, Halt) ->
  Res = exec_fun(Fun, {ok, 0}),
  if
    element(0, Res) == shutdown; Halt ->
      {_, Int} = at_exit(Res),
      erlang:halt(Int);
    true -> ok
  end.

%% spawn a new child process to run the specified fun, and monitor it
exec_fun(Fun, Res) when is_function(Fun, 1) andalso is_tuple(Res) ->
  Parent = self(),
  {Pid, Ref} = erlang:spawn_monitor(
                   fun() ->
                       try Fun(element(1, Res)) of
                           _ -> Parent ! {self(), Res}
                       catch
                         exit:{shutdown, Int} when is_integer(Int) ->
                           Parent ! {self(), {shutdown, Int}},
                           exit({shutdown, Int});
                         exit:Reason
                             when Reason == normal;
                                  Reason == shutdown;
                                  tuple_size(Reason) =:= 2, element(0, Reason) == shutdown ->
                           Parent ! {self(), {shutdown, 0}},
                           exit(Reason);
                         Kind:Reason ->
                           Stacktrace = erlang:get_stacktrace(),
                           print_error(Kind, Reason, Stacktrace),
                           Parent ! {self(), {shutdown, 1}},
                           erlang:raise(Kind, Reason, Stacktrace)
                       end
                   end),
  receive
    {Pid, Res2} ->
      erlang:demonitor(Ref, [flush]),
      Res2;
    {'DOWN', Ref, _, _, Other} ->
      print_error({'EXIT', Pid}, Other, []),
      {shutdown, 1}
  end.

%% run exit hooks on exit
at_exit(Res) ->
  Hooks = kapok_config:get(at_exit),
  Res1 = lists:foldl(fun exec_fun/2, Res, Hooks),
  {Res1, 0}.

%% Helpers

print_error(Kind, Reason, Stacktrace) ->
  io:format(standard_error, "~w: ~p~n~p~n", [Kind, Reason, Stacktrace]).

%% try to parse the shared option, if there is no mroe shared option then invoke Callback.
shared_option(List, Config, Callback) ->
  case parse_shared_args(List, #{errors := Errors} = Config) of
    {[H|T], _} when H == hd(List) ->
      Error = io_lib:format("Unknown option: ~p", [H]),
      Callback(T, Config#{errors => [Error | Errors]});
    {NewList, NewConfig} ->
      Callback(NewList, NewConfig)
  end.

%% Parse shared options
parse_shared_args([Opt | _T], _Config) when Opt == "-v"; Opt == "--version" ->
  io:format("Kapok ~s~n", [kapok_version:version()]),
  erlang:halt();
parse_shared_args(["-pa", Path | T], #{pa := PA} = Config) ->
  Paths = expand_path(Path),
  lists:map(fun code:add_patha/1, Paths),
  parse_shared_args(T, Config#{pa => PA ++ Paths});
parse_shared_args(["-pz", Path | T], #{pz := PZ} = Config) ->
  Paths = expand_path(Path),
  lists:map(fun code:add_pathz/1, Paths),
  parse_shared_args(T, Config#{pz => PZ ++ Paths});
parse_shared_args(Left, Config) ->
  {Left, Config}.


expand_path(Path) ->
  Path1 = filename:absname(expand_home(Path), cwd()),
  case wildcard(Path1) of
    [] -> [Path1];
    List -> List
  end.

wildcard(Path) ->
  filelib:wildcard(Path).

cwd() ->
  case file:get_cwd() of
    {ok, Dir} -> Dir;
    _ -> nil
  end.

expand_home(Path) ->
  Parts = filename:split(Path),
  case Parts of
    [] -> Path;
    [H|T] -> expand_home(H, T)
  end.
expand_home("~", Left) ->
  Home = os:getenv("HOME"),
  expand_home(Home, Left);
expand_home("~" ++ User, Left) ->
  Home = filename:join(filename:dirname(os:getenv("HOME")), User),
  expand_home(Home, Left);
expand_home(Home, Left) ->
  filename:join([Home | Left]).

%% Parse init options
parse_args(Args) ->
  parse_args(Args, blank_config()).
parse_args(["--" | T], Config) ->
  {Config, T};
parse_args(["+kapokc" | T], Config) ->
  parse_compiler_args(T, Config);
parse_args([H|T] = List, #{commands := Commands} = Config) ->
  case H of
    "-" ++ _Rest -> shared_option(List, Config, fun parse_args/2);
    _ -> {Config#{commands := [{file, H} | Commands]}, T}
  end;
parse_args([], Config) ->
  {Config, []}.

%% Parse compiler options

parse_compiler_args(["--" | T], Config) ->
  {Config, T};
parse_compiler_args(["-o", Outdir | T], Config) ->
  parse_compiler_args(T, Config#{outdir => Outdir});
parse_compiler_args(["--verbose" | T], Config) ->
  parse_compiler_args(T, Config#{verbose_compile => true});
parse_compiler_args([H|T] = List, #{compile := Compile} = Config) ->
  case H of
    "-" ++ _Rest ->
      shared_option(List, Config, fun parse_compiler_args/2);
    _ ->
      Pattern = case is_dir(H) of
                  true -> io_lib:format("~s/**/*.~s", [H, ?SOURCE_FILE_SUFFIX]);
                  false -> H
                end,
      parse_compiler_args(T, Config#{compile => [Pattern | Compile]})
  end;
parse_compiler_args([], #{compile := Compile, commands := Commands} = Config) ->
  {Config#{commands => [{compile, Compile} | Commands]}, []}.

is_dir(Path) ->
  kapok_utils:read_file_type(Path) == {ok, 'directory'}.

is_regular(Path) ->
  kapok_utils:read_file_type(Path) == {ok, 'regular'}.

%% Process commands
process_commands(Config) ->
  Results = lists:map(fun(C) -> process_command(C, Config) end,
                      lists:reverse(?m(Config, commands))),
  Errors = [Msg || {error, Msg} <- Results],
  lists:reverse(?m(Config, errors)) ++ Errors.

process_command({file, File}, _Config) ->
  case is_regular(File) of
    true -> exec_file(File);
    false -> {error, io_lib:format("Invalid file: ~p", [File])}
  end;
process_command({compile, Patterns}, #{outdir := Outdir} = _Config) ->
  %% ensure all parent dirs exist or be created successfully
  _ = filelib:ensure_dir(Outdir),

  case filter_multiple_patterns(Patterns) of
    {ok, []} ->
      {error, "No files matched provided pattern(s)"};
    {ok, Files} ->
      lists:map(fun(F) -> compile_file(F, Outdir) end, Files);
    {missing, Missing} ->
      {error, io_lib:format("No files matched pattern(s) ~s", [join_string_list(Missing, ",")])}
  end.

filter_pattern(Pattern) ->
  lists:filter(fun is_regular/1, lists:usort(wildcard(Pattern))).

filter_multiple_patterns(Patterns) ->
  {Match, Missing} = lists:foldl(fun(Pattern, {Match, Missing}) ->
                                     case filter_pattern(Pattern) of
                                       [] -> {Match, [Pattern | Missing]};
                                       Files -> {Files ++ Match, Missing}
                                     end
                                 end,
                                 {[], []},
                                 Patterns),
  case Missing of
    [] -> {ok, lists:usort(Match)};
    _ -> {missing, lists:usort(Missing)}
  end.

join_string_list(List, Sep) when is_list(List) ->
  L = lists:foldl(fun(E, Acc) ->
                      case Acc of
                        [] -> [E];
                        _ -> [E, Sep | Acc]
                      end
                  end,
                  [],
                  List),
  lists:foldl(fun(E, Acc) -> E++Acc end, [], L).

exec_file(File) ->
  kapok_compiler:file(list_to_binary(File)).

compile_file(File, Outdir) ->
  kapok_compiler:file(list_to_binary(File), Outdir).

