-module(process_ring).
-export([start/3, serial_start/3]).

start(M, N, Message) ->
  Pid = start_ring(N, self()),
  [ Pid ! { msg, Message } || _ <- lists:seq(1, M) ],
  Pid ! quit,
  wait().

wait() ->
  receive
    quit ->
      ok;

    _ ->
      wait()
  end.

start_ring(0, Pid) -> Pid;

start_ring(N, Pid) ->
  start_ring(N - 1, spawn(fun() -> loop(Pid) end)).

loop(Pid) ->
  receive
    { msg, _ } = Msg ->
      Pid ! Msg, loop(Pid);

    quit ->
      Pid ! quit, ok
  end.

serial_start(M, N, Message) ->
  Pid = start_ring(N, self()),
  send(Pid, M, Message),
  Pid ! quit,
  receive
    quit ->
      ok
  end.

send(_, 0, _) -> ok;

send(Pid, M, Message) ->
  Pid ! {msg, Message},

  receive
    { msg, Message } ->
      send(Pid, M - 1, Message)
  end.