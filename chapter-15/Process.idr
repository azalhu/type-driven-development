module Process

import System.Concurrency

%default total

data Message = Add Nat Nat

data MessagePID = MkMessage ThreadID

data Process : Type -> Type where
  Spawn : Process () -> Process MessagePID
  Action : IO a -> Process a
  Request : (client_chan : Channel Message) ->
            (server_chan : Channel Nat) ->
            Message -> Process Nat
  Respond : (client_chan : Channel Message) ->
            (server_chan : Channel Nat) ->
            ((msg : Message) -> Process Nat) ->
            Process Message
  Pure : a -> Process a
  (>>=) : Process a -> (a -> Process b) -> Process b
  (>>) : Process () -> Process b -> Process b

run : Process t -> IO t
run (Spawn proc) = do
  tid <- fork (run proc)
  pure (MkMessage tid)
run (Action act) = act
run (Request client_chan server_chan msg) = do
  channelPut client_chan msg
  res <- channelGet server_chan
  pure res
run (Respond client_chan server_chan calc) = do
  msg <- channelGet client_chan
  res <- run (calc msg)
  channelPut server_chan res
  pure msg
run (Pure val) = pure val
run (cmd >>= next) = run cmd >>= \res => run (next res)
run (cmd >> next) = run cmd >> run next

