module Channels

import System
import System.Concurrency

%default total

main1 : IO ()
main1 = do
  chan <- makeChannel
  threadID <- fork $ do
    channelPut chan "Hello"
    channelPut chan "Goodbye"
  putStrLn !(channelGet chan)
  putStrLn !(channelGet chan)
  pure ()

producer : Channel Nat -> IO ()
producer c = ignore $ for [1..100] $ \n => channelPut c n

mainProd : IO ()
mainProd = do
  c <- makeChannel
  tids <- for [1..10] $ \_ => fork (producer c)
  vals <- for [1..1000] $ \_ => channelGet c
  ignore $ traverse (\t => threadWait t) tids
  let s = sum vals
  if s == 50500
     then putStrLn "Success!"
     else putStrLn "How did we get here?"

data Fuel = Dry | More (Lazy Fuel)

consumer : Fuel -> Channel Nat -> IO ()
consumer Dry _ = pure ()
consumer (More fuel) c = do
  (S k) <- channelGet c
  | Z => pure ()
  consumer fuel c

covering
forever : Fuel
forever = More forever

covering
mainCons : IO ()
mainCons = do
  c <- makeChannel
  tids <- for [1..7] $ \_ => fork (consumer forever c)
  ignore $ for [1..100] $ \_ => channelPut c 1
  ignore $ for [1..7] $ \_ => channelPut c Z
  ignore $ traverse (\t => threadWait t) tids
  putStrLn "Success!"

