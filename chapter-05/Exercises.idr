module Exercises

import Data.String
import Data.Vect
import ReadNum
import System
import System.File.Handle
import System.File.ReadWrite

total
printLonger : IO ()
printLonger = do
  putStr "First string: "
  x <- getLine
  putStr "Second string: "
  y <- getLine
  let longer = max (length x) (length y)
  putStrLn (show longer)

total
printLonger' : IO ()
printLonger' =
  putStr "First string: " >>
  getLine >>= \x =>
  putStr "Second string: " >>
  getLine >>= \y =>
  let longer = max (length x) (length y) in
  putStrLn (show longer)

covering
guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
  putStr (gPrefix "Enter your answer: " True)
  Just answer <- readNumber | _ => do
    putStrLn (gPrefix "Your answer must be a number!" False)
    guess target (S guesses)
  case compare answer target of
       LT => putStrLn (gPrefix "Too low" False) >> guess target (S guesses)
       GT => putStrLn (gPrefix "Too high" False) >> guess target (S guesses)
       EQ => putStrLn (gPrefix "Your answer is correct! Hooray!" False)
  where
    gPrefix : String -> Bool -> String
    gPrefix str showGuesses = if showGuesses
                                 then "[" ++ show guesses ++ "] " ++ str
                                 else " " ++ replicate (length (show guesses)) ' ' ++ "  " ++ str

covering
mainGuess : IO ()
mainGuess = do
  target <- time
  guess (cast (mod target 101)) (S Z)

covering
my_replWith : state ->
             (prompt : String) ->
             (handle : state -> String -> Maybe (String, state)) ->
             IO ()
my_replWith state prompt handle = do
  putStr prompt
  input <- getLine
  let Just (result, state') = handle state input | Nothing => do
    pure ()
  putStrLn result
  my_replWith state' prompt handle

covering
my_repl : (prompt : String) -> (handle : String -> String) -> IO ()
my_repl prompt handle = my_replWith () prompt (\state, input => Just (handle input, state))

total
getLineMaybe : IO (Maybe String)
getLineMaybe = getLine >>= \x => pure (if x == "" then Nothing else Just x)

covering
readToBlank : IO (List String)
readToBlank = do
  Just x <- getLineMaybe
  | Nothing => pure []
  xs <- readToBlank
  pure (x :: xs)

covering
readAndSave : IO ()
readAndSave = do
  lines <- readToBlank
  file <- getLine
  Right () <- writeFile file (unlines lines)
  | Left err => putStrLn (show err)
  pure ()

covering
readVectFile : (fileName : String) -> IO (n ** Vect n String)
readVectFile fileName = do
  Right file <- openFile fileName Read
  | Left err => putStrLn (show err) >> pure (_ ** [])
  Right lines <- readFileLines file
  | Left err => putStrLn (show err) >> pure (_ ** [])
  closeFile file
  pure lines
  where
    readFileLines : (file : File) -> IO (Either FileError (n ** Vect n String))
    readFileLines file = fEOF file >>= \eof => if eof then pure (Right (_ ** [])) else do
      Right line <- fGetLine file
      | Left err => pure (Left err)
      Right (_ ** lines) <- readFileLines file
      | Left err => pure (Left err)
      pure (Right (_ ** line :: lines))

total
data Echo : String -> Type where
  MkEcho : (s : String) -> Echo s

total
echo_hi : Echo _ -> Echo "Hi"
echo_hi _ = MkEcho "Hi"

total
echo : Echo a -> Echo a
echo a = a

total
echo' :  (a : String) -> Echo a
echo' str = MkEcho str
