module ReadNum

import Data.String

total
export
readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (stringToNatOrZ input))
     else pure Nothing

total
readNumbers : IO (Maybe (Nat, Nat))
readNumbers = do
  num1 <- readNumber
  case num1 of
       Nothing => pure Nothing
       Just num1_ok => do
         num2 <- readNumber
         case num2 of
              Nothing => pure Nothing
              Just num2_ok => pure (Just (num1_ok, num2_ok))

total
readNumbers' : IO (Maybe (Nat, Nat))
readNumbers' = do
  Just num1 <- readNumber | _ => pure Nothing
  Just num2 <- readNumber | _ => pure Nothing
  pure (Just (num1, num2))

total
readPair : IO (String, String)
readPair = do
  str1 <- getLine
  str2 <- getLine
  pure (str1, str2)

total
usePair : IO ()
usePair = do
  (str1, str2) <- readPair
  putStrLn ("You entered " ++ str1 ++ " and " ++ str2)

total
fromPair : Pair ty ty -> List ty
fromPair (left, right) = [left, right]
