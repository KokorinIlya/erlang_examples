-module(mailbox_controller).
-author("kokorin-ilya").

-export([start/0]).

actor_event_loop(ReadyToReceive) ->
  receive
    clear ->
      clear_mailbox(),
      actor_event_loop(ReadyToReceive);
    notready ->
      actor_event_loop(false);
    ready ->
      actor_event_loop(true);
    Any when ReadyToReceive ->
      io:format("Received ~p~n", [Any]),
      actor_event_loop(ReadyToReceive)
  end.

clear_mailbox() ->
  receive
    _ -> clear_mailbox()
  after 0 -> void
  end.

sleep(MillisecondsToSleep) ->
  receive
  after MillisecondsToSleep -> void
  end.

main_process_event_loop(ActorRef) ->
  receive
    {sleep, MillisecondsToSleep} ->
      io:format("Sleeping... ~n"),
      sleep(MillisecondsToSleep),
      io:format("Awake! ~n"),
      main_process_event_loop(ActorRef);
    Any ->
      ActorRef ! Any,
      main_process_event_loop(ActorRef)
  end.

start() ->
  ActorRef = spawn(fun() -> actor_event_loop(false) end),
  spawn(fun() -> main_process_event_loop(ActorRef) end).
