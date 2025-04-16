module RunIO

import Data.String

%default total

data RunIO : Type -> Type where
  Quit : a ->
         RunIO a
  Do : IO a ->
       (a -> Inf (RunIO b)) ->
       RunIO b
  Seq : IO () ->
        Inf (RunIO b) ->
        RunIO b

(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

(>>) : IO () -> Inf (RunIO b) -> RunIO b
(>>) = Seq

greet : RunIO ()
greet = do
  putStr "Enter your name: "
  name <- getLine
  if name == ""
     then do
       putStrLn "Bye bye!"
       Quit ()
     else do
       putStrLn ("Hello " ++ name)
       greet

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> RunIO a -> IO (Maybe a)
run Dry _ = putStrLn "Out of fuel" >> pure Nothing
run _ (Quit value) = pure (Just value)
run (More fuel) (Do action cont) = do
  res <- action
  run fuel (cont res)
run (More fuel) (Seq action cont) = do
  action
  run fuel cont

--menu : RunIO ()
--menu = do
--  putStr "Choose your game!\n\n[g]reet\n[q]uit\n\n> "
--  game <- getLine
--  case toLower (substr 0 1 (trim game)) of
--       "g" => greet
--       "q" => Quit ()
--       _ => putStrLn "Unknown game..." >> menu

------------------------ non-total

covering
forever : Fuel
forever = More forever

covering
main : IO ()
main = run forever greet >>= \_ => pure ()

