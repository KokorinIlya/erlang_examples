-module(ticking_clocks).
-author("kokorin-ilya").

-export([start/1]).

clock_event_loop(MilliSecondsToTick, TotalMillisecondPassed) ->
  receive
    stop -> void
  after MilliSecondsToTick ->
    NewTotalMilliseconds = TotalMillisecondPassed + MilliSecondsToTick,
    io:format("Tick! ~p millisecond passed. ~n", [NewTotalMilliseconds]),
    clock_event_loop(MilliSecondsToTick, NewTotalMilliseconds)
  end.

main_process_event_loop(ClockPid) ->
  receive
    stop ->
      ClockPid ! stop
  end.

start(MilliSecondsToTick) ->
  InitClockPid = spawn(fun() -> clock_event_loop(MilliSecondsToTick, 0) end),
  spawn(fun() -> main_process_event_loop(InitClockPid) end).
