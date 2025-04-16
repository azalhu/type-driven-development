module ProcessState

import System.Concurrency

%default total

data Message = Add Nat Nat

data MessagePID = MkMessage ThreadID

namespace Channel
  export
  data ChannelState : Type -> Type where
    Idle : ChannelState ()
    Listening : ChannelState msg
    Sending : msg -> ChannelState ()

  data ChannelCmd : (ty : Type) ->
                    ChannelState a ->
                    (ty -> ChannelState b) ->
                    Type where
    Listen : ChannelCmd () Idle (const Listening)
    Read : ChannelCmd msg Listening (const Idle)
    Send : msg -> ChannelCmd () Idle (const (Sending msg))
    Write : ChannelCmd () (Sending msg) (const Idle)
    ----------
    Pure : a -> ChannelCmd a state (const state)
    (>>=) : ChannelCmd a state1 state2_fn ->
            ((res : a) -> ChannelCmd b (state2_fn res) state3_fn) ->
            ChannelCmd b state1 state3_fn
    (>>) : ChannelCmd () state1 state2_fn ->
           ChannelCmd b (state2_fn ()) state3_fn ->
           ChannelCmd b state1 state3_fn

namespace Process
  export
  data ProcessState : Type where
    NotRunning : ProcessState
    Running : (client_chan : ChannelState client_msg) ->
              (server_chan : ChannelState server_msg) ->
              ProcessState

  public export
  data ProcessCmd : (ty : Type) ->
                    ProcessState ->
                    (ty -> ProcessState) ->
                    Type where
    Spawn : ProcessCmd () NotRunning (const (Running client_chan server_chan)) ->
            ProcessCmd MessagePID NotRunning (const (Running client_chan server_chan))
    Action : IO a -> ProcessCmd a (Running c s) (const (Running c s))
    Request : Message ->
              ProcessCmd Nat (Running c s) (const (Running c s))
    Respond : ((msg : Message) -> ProcessCmd Nat (Running c s) (const (Running c s))) ->
              ProcessCmd Message (Running c s) (const (Running c s))
    ----------
    Pure : a -> ProcessCmd a state (const state)
    (>>=) : ProcessCmd a state1 state2_fn ->
            ((res : a) -> ProcessCmd b (state2_fn res) state3_fn) ->
            ProcessCmd b state1 state3_fn
    (>>) : ProcessCmd () state1 state2_fn ->
           ProcessCmd b (state2_fn ()) state3_fn ->
           ProcessCmd b state1 state3_fn

run : ProcessCmd t state1 state2 -> IO t
run (Spawn proc) = do
  tid <- fork (run proc)
  pure (MkMessage tid)
run (Action act) = act
run p = ?rhs_rest
--run : Process t -> IO t
--run (Spawn proc) = do
--  tid <- fork (run proc)
--  pure (MkMessage tid)
--run (Action act) = act
--run (Request client_chan server_chan msg) = do
--  channelPut client_chan msg
--  res <- channelGet server_chan
--  pure res
--run (Respond client_chan server_chan calc) = do
--  msg <- channelGet client_chan
--  res <- run (calc msg)
--  channelPut server_chan res
--  pure msg
--run (Pure val) = pure val
--run (cmd >>= next) = run cmd >>= \res => run (next res)
--run (cmd >> next) = run cmd >> run next

