module ReadVect

import Data.Vect

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = getLine >>= \x => readVectLen k >>= \xs => pure(x :: xs)
--readVectLen (S k) = readVectLen k >>= \xs => getLine >>= \x => pure(x :: xs)

readVectLen' : (len : Nat) -> IO (Vect len String)
readVectLen' Z = pure []
readVectLen' (S k) = do
  x <- getLine
  xs <- readVectLen' k
  pure(x :: xs)

data VectUnknown : Type -> Type where
  MkVect : (len : Nat) ->
           (xs : Vect len a) ->
           VectUnknown a

readVect : IO (VectUnknown String)
readVect = do
  x <- getLine
  if x == ""
     then pure (MkVect _ [])
     else do
       MkVect _ xs <- readVect
       pure (MkVect _ (x :: xs))

printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect len xs) = putStrLn (show xs ++ " (length " ++ show len ++ ")")

readVect' : IO (len ** Vect len String)
readVect' = do
  x <- getLine
  if x == ""
     then pure (_ ** [])
     else do
       (_ ** xs) <- readVect'
       pure (_ ** x :: xs)

printVect' : Show a => (len ** Vect len a) -> IO ()
printVect' (len ** xs) = putStrLn (show xs ++ " (length " ++ show len ++ ")")

zipInputs : IO ()
zipInputs = do
  putStrLn "Enter vect 1: "
  (n ** xs) <- readVect'
  putStrLn "Enter vect 2: "
  (m ** ys) <- readVect'
  case exactLength n ys of
       Nothing => putStrLn "error"
       Just ys' => printLn (zip xs ys')

myPair : (Nat, String)
myPair = (5, "Pages")

anyPair : (n ** Vect n Char)
anyPair = (5 ** ('P' :: 'a' :: 'g' :: 'e' :: 's' :: []))

anyVect : (n ** Vect n String)
anyVect = (3 ** ?anyVect_rhs)

