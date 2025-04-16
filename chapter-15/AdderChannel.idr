module AdderChannel

import System.Concurrency

%default total

data Message = Add Nat Nat

data Fuel = Dry | More (Lazy Fuel)

adder : Fuel ->  Channel Message -> Channel Nat -> IO ()
adder Dry _ _ = pure ()
adder (More fuel) receiver_chan sender_chan = do
  (Add x y) <- channelGet receiver_chan
  channelPut sender_chan (x + y)
  adder fuel receiver_chan sender_chan

covering
forever : Fuel
forever = More forever

covering
main : IO ()
main = do
  sender_chan <- makeChannel
  receiver_chan <- makeChannel
  adder_tid <- fork (adder forever sender_chan receiver_chan)
  channelPut sender_chan (Add 2 3)
  answer <- channelGet receiver_chan
  printLn answer

