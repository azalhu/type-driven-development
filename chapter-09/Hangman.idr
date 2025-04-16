module Hangman

import Data.String
import Data.Vect
import Data.Vect.Elem
import Decidable.Equality
import RemoveElem

total
data WordState : (guesses_remaining : Nat) -> (letters : Nat) -> Type where
  MkWordState : (word : String) ->
                (missing : Vect letters Char) ->
                WordState guesses_remaining letters

total
data Finished : Type where
  Lost : (game : WordState Z (S letters)) -> Finished
  Won : (game : WordState (S guesses) Z) -> Finished

total
data ValidInput : List Char -> Type where
  Letter : (c : Char) -> ValidInput (c :: Nil)

total
inputIsNil : ValidInput Nil -> Void
inputIsNil _ impossible

total
inputIsChars : ValidInput (d :: c :: cs) -> Void
inputIsChars _ impossible

total
isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput Nil = No inputIsNil
isValidInput (c :: Nil) = Yes (Letter c)
isValidInput (d :: c :: cs) = No inputIsChars

total
isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)

total
processGuess : {letters : _} ->
               (letter : Char) ->
               WordState (S guesses) (S letters) ->
               Either (WordState guesses (S letters)) (WordState (S guesses) letters)
processGuess letter (MkWordState word missing) =
  case isElem letter missing of
       Yes prf => Right (MkWordState word (removeElem letter missing))
       No contra => Left (MkWordState word missing)

covering
readGuess : IO (x ** ValidInput x)
readGuess = do
  putStr "Guess: "
  x <- getLine
  case isValidString (toUpper x) of
       Yes prf => pure (_ ** prf)
       No contra => do
         putStrLn "Invalid guess"
         readGuess

covering
game : {guesses : _} ->
       {letters : _} ->
       WordState (S guesses) (S letters) ->
       IO Finished
game st = do
  ((x :: Nil) ** prf) <- readGuess
  case processGuess x st of
       Left l => do
         putStrLn "Wrong!"
         case guesses of
              Z => pure (Lost l)
              S more => game l
       Right r => do
         putStrLn "Right!"
         case letters of
              Z => pure (Won r)
              S more => game r

covering
main : IO ()
main = do
  result <- game {guesses=2} (MkWordState "Test" ['T', 'E', 'S'])
  case result of
       Lost (MkWordState word missing) => putStrLn ("You lose. The word was " ++ word)
       Won game => putStrLn "You win!"

