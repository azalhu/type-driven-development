module Loops

import System
import ReadNum

total
countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown (S secs) = do
  putStrLn (show (S secs))
  sleep 1
  countdown secs

covering
countdowns : IO ()
countdowns = do
  putStr "Enter starting number: "
  Just startNum <- readNumber | _ => do
    putStrLn "Invalid input"
    countdowns
  countdown startNum
  putStr "Another (y/n)? "
  yn <- getLine
  if yn == "y"
     then countdowns
     else pure ()

