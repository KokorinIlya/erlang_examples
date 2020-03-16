-module(remote_calculator).
-author("kokorin-ilya").

-export([start/0]).

priority_receive() ->
  receive
    {setworing, _} = Msg -> Msg;
    {setsleeptime, _} = Msg -> Msg
  after 0 ->
    receive
      {sync, sum, _, _, _} = Msg -> Msg
    after 0 ->
      receive
        {async, sum, _, _, _, _} = Msg -> Msg;
        Any -> Any
      end
    end
  end.

server_event_loop(SleepTime) ->
  case priority_receive() of
    {setsleeptime, Value} ->
      server_event_loop(Value);
    {sync, sum, A, B, FromPid} ->
      Result = A + B,
      timer:sleep(SleepTime),
      FromPid ! {syncans, Result, self()},
      io:format("Finished remote sync calculation: ~p + ~p = ~p~n", [A, B, Result]),
      server_event_loop(SleepTime);
    {async, sum, A, B, MessageRef, FromPid} ->
      Result = A + B,
      timer:sleep(SleepTime),
      FromPid ! {asyncans, Result, MessageRef, self()},
      io:format("Finished remote async calculation: ~p + ~p = ~p~n", [A, B, Result]),
      server_event_loop(SleepTime)
  end.

client_event_loop(ServerPid, Requests) ->
  receive
    {calcsync, A, B} ->
      ServerPid ! {sync, sum, A, B, self()},
      receive
        {syncans, Result, _} ->
          io:format("Sync: ~p + ~p = ~p~n", [A, B, Result])
      end,
      client_event_loop(ServerPid, Requests);
    {calcasync, A, B} ->
      MessageRef = make_ref(),
      NewRequests = maps:put(MessageRef, {sum, A, B}, Requests),
      ServerPid ! {async, sum, A, B, MessageRef, self()},
      client_event_loop(ServerPid, NewRequests);
    {asyncans, Result, MessageRef, ServerPid} ->
      {sum, A, B} = maps:get(MessageRef, Requests),
      io:format("Async: ~p + ~p = ~p~n", [A, B, Result]),
      NewRequests = maps:remove(MessageRef, Requests),
      client_event_loop(ServerPid, NewRequests);
    {setsleeptime, _} = Msg ->
      ServerPid ! Msg,
      client_event_loop(ServerPid, Requests);
    ping ->
      io:format("Pong!~n"),
      client_event_loop(ServerPid, Requests)
  end.

start() ->
  ServerPid = spawn(fun() -> server_event_loop(1000) end),
  spawn(fun() -> client_event_loop(ServerPid, maps:new()) end).
