-module(fail_calc).
-author("kokorin-ilya").

-export([start/2]).

worker_event_loop() ->
  receive
    ping ->
      io:format("Pong!~n"),
      worker_event_loop();
    fail ->
      io:format("Failing~n"),
      1 div 0;
    stop ->
      io:format("Exciting~n"),
      void
  end.

main_event_loop(ActorsCount, ActorsMap) ->
  CurMapSize = maps:size(ActorsMap),
  if CurMapSize < ActorsCount ->
    CurPid = spawn_link(fun worker_event_loop/0),
    NewMap = maps:put(CurMapSize, CurPid, ActorsMap),
    main_event_loop(ActorsCount, NewMap);
    true ->
      receive
        {'EXIT', Pid, Why} ->
          io:format("Process ~p exited with ~p~n", [Pid, Why]),
          main_event_loop(ActorsCount, ActorsMap);
        ping ->
          io:format("Pong!~n"),
          main_event_loop(ActorsCount, ActorsMap);
        {stop, ProcessNum} when ProcessNum >= 0 andalso ProcessNum < ActorsCount ->
          Pid = maps:get(ProcessNum, ActorsMap),
          Pid ! stop,
          main_event_loop(ActorsCount, ActorsMap);
        {fail, ProcessNum} when ProcessNum >= 0 andalso ProcessNum < ActorsCount ->
          Pid = maps:get(ProcessNum, ActorsMap),
          Pid ! fail,
          main_event_loop(ActorsCount, ActorsMap);
        {ping, ProcessNum} when ProcessNum >= 0 andalso ProcessNum < ActorsCount ->
          Pid = maps:get(ProcessNum, ActorsMap),
          Pid ! ping,
          main_event_loop(ActorsCount, ActorsMap)
      end
  end.

start(IsSystem, ActorsCount) ->
  spawn(
    fun() ->
      process_flag(trap_exit, IsSystem),
      main_event_loop(ActorsCount, maps:new())
    end
  ).
