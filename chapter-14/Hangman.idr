module Hangman

import Data.String
import Data.Vect
import Data.Vect.Elem
import Decidable.Equality

%default total

---------- Old implementation - rules not followed

data WordState : (guesses : Nat) -> (letters : Nat) -> Type where
  MkWordState : (word : String) ->
                (missing : Vect letters Char) ->
                WordState guesses_remaining letters

data Finished : Type where
  Lost' : (game : WordState Z (S letters)) -> Finished
  Won' : (game : WordState (S guesses) Z) -> Finished

game : WordState (S guesses) (S letters) -> IO Finished
game state = pure (Lost' (MkWordState "ANYTHING" ['A']))

---------- New implementation - rules followed

data GameState : Type where
  NotRunning : GameState
  Running : (guesses : Nat) -> (letters : Nat) -> GameState

letters : String -> List Char
letters str = nub (map toUpper (unpack str))

data GuessResult = Correct | Incorrect

data GameCmd : (ty : Type) ->
               GameState ->
               (ty -> GameState) ->
               Type where
  NewGame : (word : String) ->
            GameCmd () NotRunning (const (Running 6 (length (letters word))))
  Guess : (letter : Char) ->
          GameCmd GuessResult (Running (S guesses) (S letters'))
          (\res => case res of
          Correct => Running (S guesses) letters'
          Incorrect => Running guesses (S letters'))
  Won : GameCmd () (Running (S guesses) Z) (const NotRunning)
  Lost : GameCmd () (Running Z (S letters')) (const NotRunning)
  ----------
  ShowState : GameCmd () state (const state)
  Message : String -> GameCmd () state (const state)
  ReadGuess : GameCmd Char state (const state)
  ----------
  Pure : (res : ty) -> GameCmd ty (state_fn res) state_fn
  (>>=) : GameCmd a state1 state2_fn ->
          ((res : a) -> GameCmd b (state2_fn res) state3_fn) ->
          GameCmd b state1 state3_fn
  (>>) : GameCmd () state1 state2_fn ->
         GameCmd b (state2_fn ()) state3_fn ->
         GameCmd b state1 state3_fn

data Game : GameState -> Type where
  GameStart : Game NotRunning
  GameWon : (word : String) -> Game NotRunning
  GameLost : (word : String) -> Game NotRunning
  InProgress : {letters : _} ->
               (word : String) ->
               (guesses : Nat) ->
               (missing : Vect letters Char) ->
               Game (Running guesses letters)

Show (Game st) where
  show GameStart = "Starting"
  show (GameWon word) = "Game won: word was " ++ word
  show (GameLost word) = "Game lost: word was " ++ word
  show (InProgress word guesses missing) =
    "\n" ++ pack (map hideMissing (unpack word)) ++
    "\n" ++ show guesses ++ " guesses left"
    where
      hideMissing : Char -> Char
      hideMissing c = if c `elem` missing then '-' else c

namespace Loop
  public export
  data GameLoop : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
    (>>=) : GameCmd a state1 state2_fn ->
            ((res : a) -> Inf (GameLoop b (state2_fn res) state3_fn)) ->
            GameLoop b state1 state3_fn
    (>>) : GameCmd () state1 state2_fn ->
           Lazy (GameLoop b (state2_fn ()) state3_fn) ->
           GameLoop b state1 state3_fn
    Exit : GameLoop () NotRunning (const NotRunning)

gameLoop : {guesses : _} ->
           {letters' : _} ->
           GameLoop () (Running (S guesses) (S letters')) (const NotRunning)
gameLoop = do
  ShowState
  g <- ReadGuess
  Correct <- Guess g
  | Incorrect => do
    case guesses of
         Z => do
           Lost
           ShowState
           Exit
         S k => do
           Message "Incorrect"
           gameLoop
  case letters' of
       Z => do
         Won
         ShowState
         Exit
       S k => do
         Message "Correct"
         gameLoop

hangman : GameLoop () NotRunning (const NotRunning)
hangman = do
  NewGame "testing"
  gameLoop

data Fuel = Dry | More (Lazy Fuel)

data GameResult : (ty : Type) -> (ty -> GameState) -> Type where
  OK : (res : ty) -> Game (outstate_fn res) -> GameResult ty outstate_fn
  OutOfFuel : GameResult ty outstate_fn

ok : (res : ty) -> Game (outstate_fn res) -> IO (GameResult ty outstate_fn)
ok res st = pure (OK res st)

runCmd : Fuel ->
         Game instate ->
         GameCmd ty instate outstate_fn ->
         IO (GameResult ty outstate_fn)
runCmd Dry _ _ = pure OutOfFuel
runCmd fuel state (NewGame word) = ok () (InProgress (toUpper word) _ (fromList (letters word)))
runCmd fuel (InProgress word _ missing) Won = ok () (GameWon word)
runCmd fuel (InProgress word _ missing) Lost = ok () (GameLost word)
runCmd fuel (InProgress word _ missing) (Guess c) = do
  case isElem c missing of
       Yes prf => ok Correct (InProgress word _ (dropElem missing prf))
       No contra => ok Incorrect (InProgress word _ missing)
runCmd fuel state ShowState = do
  printLn state
  ok () state
runCmd fuel state (Message str) = do
  putStrLn str
  ok () state
runCmd (More fuel) state ReadGuess = do
  putStr "Guess: "
  input <- getLine
  case unpack input of
       [x] => if isAlpha x
                 then ok (toUpper x) state
                 else do
                   putStrLn "Invalid input"
                   runCmd fuel state ReadGuess
       _ => do
         putStrLn "Invalid input"
         runCmd fuel state ReadGuess
runCmd fuel state (Pure res) = ok res state
runCmd fuel state (cmd >>= next) = do
  OK cmdRes newState <- runCmd fuel state cmd
  | OutOfFuel => pure OutOfFuel
  runCmd fuel newState (next cmdRes)
runCmd fuel state (cmd >> next) = do
  OK () newState <- runCmd fuel state cmd
  | OutOfFuel => pure OutOfFuel
  runCmd fuel newState next

run : Fuel ->
      Game instate ->
      GameLoop ty instate outstate_fn ->
      IO (GameResult ty outstate_fn)
run Dry _ _ = pure OutOfFuel
run (More fuel) st (cmd >>= next) = do
  OK cmdRes newSt <- runCmd fuel st cmd
  | OutOfFuel => pure OutOfFuel
  run fuel newSt (next cmdRes)
run (More fuel) st (cmd >> next) = do
  OK () newSt <- runCmd fuel st cmd
  | OutOfFuel => pure OutOfFuel
  run fuel newSt next
run (More fuel) st Exit = ok () st

%default covering

forever : Fuel
forever = More forever

main : IO ()
main = do
  _ <- run forever GameStart hangman
  pure ()

