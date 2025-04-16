module ArithCmdDo

import Data.Bits
import Data.Primitives.Views
import Data.String
import System
import System.File
import System.File.Error

%default total


---------- Command


data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String
  ReadFile : String -> Command (Either FileError String)
  WriteFile : String -> String -> Command (Either FileError ())
  Pure : ty -> Command ty
  Bind : Command a -> (a -> Command b) -> Command b

Functor Command where
  map f c = Bind c (Pure . f)

Applicative Command where
  pure = Pure
  (<*>) f c = Bind f (flip map c)

Monad Command where
  (>>=) = Bind


---------- ConsoleIO


data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  Seq : Command () -> Inf (ConsoleIO b) -> ConsoleIO b

namespace ConsoleDo
  export
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

  export
  (>>) : Command () -> Inf (ConsoleIO b) -> ConsoleIO b
  (>>) = Seq


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


record QuizState where
  constructor MkQuizState
  score : Nat
  questions : Nat

Show QuizState where
  show state = show (score state) ++ " / " ++ show (questions state)

mutual
  correct : Stream Int -> QuizState -> ConsoleIO QuizState
  correct nums (MkQuizState score questions) = do
    PutStr "Correct!\n"
    quiz nums (MkQuizState (S score) (S questions))

  wrong : Stream Int -> Int -> QuizState -> ConsoleIO QuizState
  wrong nums ans (MkQuizState score questions) = do
    PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
    quiz nums (MkQuizState score (S questions))

  quiz : Stream Int -> QuizState -> ConsoleIO QuizState
  quiz (num1 :: num2 :: nums) state = do
    PutStr ("Score so far: " ++ show state ++ "\n")
    input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
    case input of
         QuitCmd => Quit state
         Answer answer => if answer == num1 * num2
                             then correct nums state
                             else wrong nums (num1 * num2) state


startQuiz : Stream Int -> ConsoleIO QuizState
startQuiz nums = quiz nums (MkQuizState Z Z)

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound num with (divides num 12)
      bound ((12 * div) + rem) | (DivBy div rem prf) = rem + 1


---------- Menu


menu : IO ()


---------- Run


data Fuel' = Dry | More (Lazy Fuel')

covering
runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (ReadFile filePath) = readFile filePath
runCommand (WriteFile filePath contents) = writeFile filePath contents
runCommand (Pure val) = pure val
runCommand (Bind c f) = runCommand c >>= runCommand . f

covering
run' : Fuel' -> ConsoleIO a -> IO (Maybe a)
run' Dry _ = pure Nothing
run' _ (Quit value) = pure (Just value)
run' (More fuel) (Do command next) = do
  res <- runCommand command
  run' fuel (next res)
run' (More fuel) (Seq command next) = do
  runCommand command
  run' fuel next


---------- Program


covering
forever' : Fuel'
forever' = More forever'

covering
main : IO ()
main = do
  seed <- time
  Just score <- run' forever' (startQuiz (arithInputs (fromInteger seed)))
  | Nothing => putStrLn "Ran out of fuel"
  putStrLn ("Final score: " ++ show score)

