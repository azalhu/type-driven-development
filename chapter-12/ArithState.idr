module ArithState

import Data.Bits
import Data.Primitives.Views
import Data.String
import System

%default total

---------- GameState

record Score where
  constructor MkScore
  correct : Nat
  attempted : Nat

Show Score where
  show score =
    show (correct score) ++ "/" ++
    show (attempted score)

record GameState where
  constructor MkGameState
  score : Score
  difficulty : Int

Show GameState where
  show st =
    show (score st) ++ "\n" ++
    "Difficulty: " ++ show (difficulty st)

initState : GameState
initState = MkGameState (MkScore Z Z) 12

setDifficulty : Nat -> GameState -> GameState
setDifficulty newDiff = {
  difficulty := cast newDiff
}

addWrong : GameState -> GameState
addWrong = {
  score->attempted $= (+1)
}

addCorrect : GameState -> GameState
addCorrect = {
  score->correct $= (+1),
  score->attempted $= (+1)
}

---------- Command

data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String
  ----------------------
  GetRandom : Command Int
  GetGameState : Command GameState
  PutGameState : GameState -> Command ()
  ----------------------
  Pure : ty -> Command ty
  Bind : Command a -> (a -> Command b) -> Command b

Functor Command where
  map f c = Bind c (Pure . f)

Applicative Command where
  pure = Pure
  (<*>) f c = Bind f (flip map c)

Monad Command where
  (>>=) = Bind

---------- CommandRun

runCommand : Stream Int ->
             GameState ->
             Command a ->
             IO (a, Stream Int, GameState)
runCommand rnds state (PutStr x) = putStr x >> pure ((), rnds, state)
runCommand rnds state GetLine = getLine >>= \str => pure (str, rnds, state)
runCommand (val :: rnds) state GetRandom =
  pure (getRandom val (difficulty state), rnds, state)
  where
    getRandom : Int -> Int -> Int
    getRandom val max with (divides val max)
      getRandom val 0 | DivByZero = 1
      getRandom ((max * val) + rem) max | DivBy _ _ prf = abs rem + 1
runCommand rnds state GetGameState = pure (state, rnds, state)
runCommand rnds state (PutGameState newState) = pure ((), rnds, newState)
runCommand rnds state (Pure val) = pure (val, rnds, state)
runCommand rnds state (Bind c f) = do
  (res, newRnds, newState) <- runCommand rnds state c
  runCommand newRnds newState (f res)

---------- ConsoleIO

data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  Seq : Command () -> Lazy (ConsoleIO b) -> ConsoleIO b

namespace ConsoleDo
  export
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

  export
  (>>) : Command () -> Lazy (ConsoleIO b) -> ConsoleIO b
  (>>) = Seq

---------- ConsoleRun

data Fuel = Dry | More (Lazy Fuel)

run : Fuel ->
      Stream Int ->
      GameState ->
      ConsoleIO a ->
      IO (Maybe a, Stream Int, GameState)
run Dry rnds state p = pure (Nothing, rnds, state)
run fuel rnds state (Quit val) = pure (Just val, rnds, state)
run (More fuel) rnds state (Do c f) = do
  (res, newRnds, newState) <- runCommand rnds state c
  run fuel newRnds newState (f res)
run (More fuel) rnds state (Seq c f) = do
  ((), newRnds, newState) <- runCommand rnds state c
  run fuel newRnds newState f

---------- Input

data Input = Answer Int | QuitCmd

readInput : (prompt : String) -> Command Input
readInput prompt = do
  PutStr prompt
  answer <- GetLine
  if toLower answer == "quit"
     then Pure QuitCmd
     else Pure (Answer (cast answer))

---------- Quiz

updateGameState : (GameState -> GameState) -> Command ()
updateGameState f = do
  st <- GetGameState
  PutGameState (f st)

mutual
  correct : ConsoleIO GameState
  correct = do
    PutStr ("Correct!" ++ "\n")
    updateGameState addCorrect
    quiz

  wrong : Int -> ConsoleIO GameState
  wrong ans = do
    PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
    updateGameState addWrong
    quiz

  quiz : ConsoleIO GameState
  quiz = do
    num1 <- GetRandom
    num2 <- GetRandom
    st <- GetGameState
    PutStr (show st ++ "\n")
    input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
    case input of
         QuitCmd => Quit st
         Answer answer => let correct_answer = num1 * num2 in
                              if answer == correct_answer
                                 then correct
                                 else wrong correct_answer

---------- Program

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

covering
forever : Fuel
forever = More forever

covering
main : IO ()
main = do
  seed <- time
  (Just score, _, state) <- run forever (randoms (fromInteger seed)) initState quiz
  | _ => putStrLn "Ran out of fuel"
  putStrLn ("Final score: " ++ show score)

