-module(ceiba_code_server).
-export([call/1, cast/1]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).

-define(timeout, 30000).
-record(ceiba_code_server, {
            loaded=[],
            mod_pool={[], 0},
            mod_ets=dict:new(),
            compilation_status=[]
           }).

call(Args) ->
  gen_server:call(?MODULE, Args, ?timeout).

cast(Args) ->
  gen_server:cast(?MODULE, Args).

%% Callbacks

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).

init(ok) ->
  %% The table where we store module definitions
  _ = ets:new(ceiba_modules, [set, protected, named_table, {read_concurrency, true}]),

  {ok, #ceiba_code_server{}}.

handle_call({defmodule, Pid, Tuple}, _From, Config) ->
  {Ref, New} = defmodule(Pid, Tuple, Config),
  {reply, Ref, New};

handle_call({undefmodule, Ref}, _From, Config) ->
  {reply, ok, undefmodule(Ref, Config)};

handle_call({acquire, Path}, From, Config) ->
  Current = Config#ceiba_code_server.loaded,
  case orddict:find(Path, Current) of
    {ok, true} ->
      {reply, loaded, Config};
    {ok, {Ref, List}} when is_list(List), is_reference(Ref) ->
      Queued = orddict:store(Path, {Ref, [From|List]}, Current),
      {reply, {queued, Ref}, Config#ceiba_code_server{loaded=Queued}};
    error ->
      Queued = orddict:store(Path, {make_ref(), []}, Current),
      {reply, proceed, Config#ceiba_code_server{loaded=Queued}}
  end;

handle_call(loaded, _From, Config) ->
  {reply, [F || {F, true} <- Config#ceiba_code_server.loaded], Config};

handle_call({compilation_status, CompilerPid}, _From, Config) ->
  CompilationStatusList = Config#ceiba_code_server.compilation_status,
  CompilationStatusListNew = orddict:erase(CompilerPid, CompilationStatusList),
  CompilationStatus = orddict:fetch(CompilerPid, CompilationStatusList),
  {reply, CompilationStatus,
   Config#ceiba_code_server{compilation_status=CompilationStatusListNew}};

handle_call(retrieve_module_name, _From, Config) ->
  case Config#ceiba_code_server.mod_pool of
    {[H|T], Counter} ->
      {reply, module_tuple(H), Config#ceiba_code_server{mod_pool={T, Counter}}};
    {[], Counter} ->
      {reply, module_tuple(Counter), Config#ceiba_code_server{mod_pool={[], Counter+1}}}
  end;

handle_call(Request, _From, Config) ->
  {stop, {badcall, Request}, Config}.

handle_cast({register_warning, CompilerPid}, Config) ->
  CompilerOptions = ceiba_config:get(compiler_options),
  case orddict:find(warnings_as_errors, CompilerOptions) of
    {ok, true} ->
      CompilationStatusCurrent = Config#ceiba_code_server.compilation_status,
      CompilationStatusNew = orddict:store(CompilerPid, error, CompilationStatusCurrent),
      {noreply, Config#ceiba_code_server{compilation_status=CompilationStatusNew}};
    _ ->
      {noreply, Config}
  end;

handle_cast({reset_warnings, CompilerPid}, Config) ->
  CompilationStatusCurrent = Config#ceiba_code_server.compilation_status,
  CompilationStatusNew = orddict:store(CompilerPid, ok, CompilationStatusCurrent),
  {noreply, Config#ceiba_code_server{compilation_status=CompilationStatusNew}};

handle_cast({loaded, Path}, Config) ->
  Current = Config#ceiba_code_server.loaded,
  case orddict:find(Path, Current) of
    {ok, true} ->
      {noreply, Config};
    {ok, {Ref, List}} when is_list(List), is_reference(Ref) ->
      _ = [Pid ! {ceiba_code_server, Ref, loaded} || {Pid, _Tag} <- lists:reverse(List)],
      Done = orddict:store(Path, true, Current),
      {noreply, Config#ceiba_code_server{loaded=Done}};
    error ->
      Done = orddict:store(Path, true, Current),
      {noreply, Config#ceiba_code_server{loaded=Done}}
  end;

handle_cast({unload_files, Files}, Config) ->
  Current  = Config#ceiba_code_server.loaded,
  Unloaded = lists:foldl(fun(File, Acc) -> orddict:erase(File, Acc) end, Current, Files),
  {noreply, Config#ceiba_code_server{loaded=Unloaded}};

handle_cast({return_module_name, H}, #ceiba_code_server{mod_pool={T, Counter}} = Config) ->
  {noreply, Config#ceiba_code_server{mod_pool={[H|T], Counter}}};

handle_cast(Request, Config) ->
  {stop, {badcast, Request}, Config}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, Config) ->
  {noreply, undefmodule(Ref, Config)};

handle_info(_Msg, Config) ->
  {noreply, Config}.

terminate(_Reason, _Config) ->
  ok.

code_change(_Old, Config, _Extra) ->
  {ok, Config}.

module_tuple(I) ->
  {list_to_atom("ceiba_compiler_" ++ integer_to_list(I)), I}.

defmodule(Pid, Tuple, #ceiba_code_server{mod_ets=ModEts} = Config) ->
  ets:insert(ceiba_modules, Tuple),
  Ref = erlang:monitor(process, Pid),
  Mod = erlang:element(1, Tuple),
  {Ref, Config#ceiba_code_server{mod_ets=dict:store(Ref, Mod, ModEts)}}.

undefmodule(Ref, #ceiba_code_server{mod_ets=ModEts} = Config) ->
  case dict:find(Ref, ModEts) of
    {ok, Mod} ->
      ets:delete(ceiba_modules, Mod),
      Config#ceiba_code_server{mod_ets=dict:erase(Ref, ModEts)};
    error ->
      Config
  end.

