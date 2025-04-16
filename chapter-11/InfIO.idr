module InfIO

%default total

export
data InfIO : Type where
  Do : IO a ->
       (a -> Inf InfIO) ->
       InfIO
  Seq : IO () ->
        Inf InfIO ->
        InfIO

export
(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

export
(>>) : IO () -> Inf InfIO -> InfIO
(>>) = Seq

loopPrint : String -> InfIO
loopPrint msg = do
  putStrLn msg
  loopPrint msg

data Fuel = Dry | More (Lazy Fuel)

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)

export
run : Fuel -> InfIO -> IO ()
run Dry p = putStrLn "Out of fuel"
run (More fuel) (Do action cont) = do
  res <- action
  run fuel (cont res)
run (More fuel) (Seq action cont) = do
  action
  run fuel cont

------------------------ non-total

covering export
forever : Fuel
forever = More forever

