%% Command line functionals.
-module(kapok_cli).
-export([main/1]).
-include("kapok.hrl").


main(Args) ->
  Args1 = [kapok_utils:chardata_to_string(A) || A <- Args ],
  {Config, _} = parse_args(Args1),
  run(fun (_) ->
          Errors = process_commands(Config),
          if
            Errors =/= [] ->
              lists:map(fun (E) -> io:format(standard_error, "~p~n", [E]) end,
                        Errors);
            true -> ok
          end
      end,
      ?m(Config, halt)).

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

at_exit(Res) ->
  %% TODO hard code
  {Res, 0}.
  %%Hooks = kapok_config:get(at_exit),
  %%lists:foldl(fun exec_fun/2, Res, Hooks).

print_error(Kind, Reason, Stacktrace) ->
  io:format(standard_error, "~w~s~n~p~n",[Kind, Reason, Stacktrace]).


new_config() ->
  #{commands => [],
    compile => [],
    compiler_options => [],
    pa => [],
    pz => [],
    verbose_compile => false,
    halt => true,
    output => ".",
    errors => []
   }.

%% Parse command line arguments
parse_args(Args) ->
  parse_args(Args, new_config()).
parse_args(["-S", H | T], Config) ->
  NewConfig = Config#{commands := [{script, H} | ?m(Config, commands)]},
  parse_args(T, NewConfig);
parse_args([H|T], Config) ->
  NewConfig = Config#{commands := [{file, H} | ?m(Config, commands)]},
  parse_args(T, NewConfig);
parse_args([], Config) ->
  {Config, []}.

%% Process commands
process_commands(Config) ->
  Results = lists:map(
                fun (C) ->
                    process_command(C, Config)
                end,
                lists:reverse(?m(Config, commands))),
  Errors = [Msg || {error, Msg} <- Results],
  lists:reverse(?m(Config, errors)) ++ Errors.

process_command({script, File}, _Config) when is_binary(File) ->
  require_file(File);
process_command({file, File}, _Config) when is_binary(File) ->
  require_file(File).

require_file(File) ->
  kapok_compiler:file(File).

