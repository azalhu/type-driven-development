module ProcessLinear

import Control.Linear.LIO
import Data.String
import Data.Vect

%default total

data Error : Type where
  NoNat  : String -> Error
  NoBool : String -> Error

Show Error where
  show (NoNat msg) = msg
  show (NoBool msg) = msg

record Console where
  constructor MkConsole
  read : IO String
  put  : String -> IO ()

record ErrorHandler where
  constructor MkHandler
  handle : Error -> IO ()

parameters {auto h : ErrorHandler} {auto c : Console}
  getCount : IO (Maybe Nat)
  getCount = do
    str <- c.read
    case parsePositive str of
      Nothing => h.handle (NoNat "Not positive: \{str}") $> Nothing
      Just n  => pure (Just n)

  getText : (n : Nat) -> IO (Vect n String)
  getText n = sequence $ replicate n c.read

  prog : IO ()
  prog = do
    c.put "Please enter the number of lines to read."
    Just n  <- getCount
    | Nothing => pure ()
    c.put "Please enter \{show n} \{if n == 1 then "line" else "lines"} of text."
    ls <- getText n
    c.put "Read \{show n} \{if n == 1 then "line" else "lines"} and \{show . sum $ map length ls} characters."

0
Services : Type
Services = (ErrorHandler, Console)

services : Services
services = (MkHandler (\err => putStrLn (show err)), MkConsole getLine putStrLn)

initServices : IO Services
initServices = pure services

main : IO ()
main =
  let cons := MkConsole (trim <$> getLine) putStrLn
      err  := MkHandler (const $ putStrLn "It didn't work")
   in prog

