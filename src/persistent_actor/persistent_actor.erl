-module(persistent_actor).
-author("kokorin-ilya").

-export([start/1]).

persistent_event_loop() ->
  receive
    ping ->
      io:format("Pong!~n"),
      persistent_event_loop();
    stop ->
      io:format("Stopping...~n");
    fail ->
      io:format("Failing...~n"),
      1 / 0
  end.

supervisor_event_loop(Name) ->
  SupervisedPid = spawn_link(
    fun() ->
      io:format("Starting...~n"),
      persistent_event_loop()
    end
  ),
  io:format("Started supervised process with pid ~p~n", [SupervisedPid]),
  try
    register(Name, SupervisedPid)
  catch
    error : badarg ->
      io:format("Process with name ~p is already registered, choose another name~n", [Name]),
      SupervisedPid ! stop,
      receive
        {'EXIT', SupervisedPid, normal} ->
          io:format("Supervised process ~p has exited ~n", [SupervisedPid])
      end,
      exit(normal)
  end,
  receive
    {'EXIT', SupervisedPid, Why} ->
      io:format("Supervised process ~p exited with reason ~p~n", [SupervisedPid, Why]),
      supervisor_event_loop(Name)
  end.

start(Name) ->
  spawn(
    fun() ->
      process_flag(trap_exit, true),
      supervisor_event_loop(Name)
    end
  ).
