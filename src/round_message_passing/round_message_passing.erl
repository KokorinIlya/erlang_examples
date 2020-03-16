-module(round_message_passing).
-author("kokorin-ilya").

-export([start/1]).

process_event_loop(CurProcessNum, TotalProcesses, NumToPidMap, MessagesStartTimeMap) ->
  receive
    {init, InitNumToPidMap} when NumToPidMap =:= undefined ->
      process_event_loop(CurProcessNum, TotalProcesses, InitNumToPidMap, MessagesStartTimeMap);
    {message, InitNum, _, _} = Msg when NumToPidMap =/= undefined andalso CurProcessNum =/= InitNum ->
      NextProcessNum = (CurProcessNum + 1) rem TotalProcesses,
      NextProcessPid = maps:get(NextProcessNum, NumToPidMap),
      NextProcessPid ! Msg,
      process_event_loop(CurProcessNum, TotalProcesses, NumToPidMap, MessagesStartTimeMap);
    {message, InitNum, MessageText, MessageRef} when CurProcessNum =:= InitNum ->
      MessageStartTime = maps:get(MessageRef, MessagesStartTimeMap),
      MessageFinishTime = erlang:system_time(microsecond),
      MessageDelta = MessageFinishTime - MessageStartTime,
      io:format("Message ~p arrived in ~p microseconds ~n", [MessageText, MessageDelta]),
      NewMessagesStartTimeMap = maps:remove(MessageRef, MessagesStartTimeMap),
      process_event_loop(CurProcessNum, TotalProcesses, NumToPidMap, NewMessagesStartTimeMap);
    {send, Message} when NumToPidMap =/= undefined ->
      io:format("Sending message ~p~n", [Message]),
      StartTime = erlang:system_time(microsecond),
      CurMessageRef = make_ref(),
      NewMessagesStartTimeMap = maps:put(CurMessageRef, StartTime, MessagesStartTimeMap),
      Msg = {message, CurProcessNum, Message, CurMessageRef},
      NextProcessNum = (CurProcessNum + 1) rem TotalProcesses,
      NextProcessPid = maps:get(NextProcessNum, NumToPidMap),
      NextProcessPid ! Msg,
      process_event_loop(CurProcessNum, TotalProcesses, NumToPidMap, NewMessagesStartTimeMap)
  end.

start_processes(CurProcessNum, TotalProcessesToSpawn, NumToPidMap) when CurProcessNum >= TotalProcessesToSpawn ->
  NumToPidMap;
start_processes(CurProcessNum, TotalProcessesToSpawn, NumToPidMap) when CurProcessNum < TotalProcessesToSpawn ->
  CurPid = spawn(fun() -> process_event_loop(CurProcessNum, TotalProcessesToSpawn, undefined, maps:new()) end),
  NewMap = maps:put(CurProcessNum, CurPid, NumToPidMap),
  start_processes(CurProcessNum + 1, TotalProcessesToSpawn, NewMap).

main_process_event_loop(NumToPidMap) ->
  receive
    {send, MessageText, ProcessNum} ->
      Pid = maps:get(ProcessNum, NumToPidMap),
      Pid ! {send, MessageText},
      main_process_event_loop(NumToPidMap)
  end.

start(TotalProcessesToSpawn) ->
  ResultMap = start_processes(0, TotalProcessesToSpawn, maps:new()),
  lists:foreach(fun(CurPid) -> CurPid ! {init, ResultMap} end, maps:values(ResultMap)),
  io:format("Finished building actor system ~n"),
  spawn(fun() -> main_process_event_loop(ResultMap) end).
