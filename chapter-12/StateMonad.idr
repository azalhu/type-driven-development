module StateMonad

%default total

-------------------------- State

data State : (stateType : Type) -> Type -> Type where
  Get : State stateType stateType
  Put : stateType -> State stateType ()
  ---------------
  Pure : a -> State stateType a
  Bind : State stateType a -> (a -> State stateType b) -> State stateType b

get : State stateType stateType
get = Get

put : stateType -> State stateType ()
put = Put

Functor (State stateType) where
  map f s = Bind s (Pure . f)

Applicative (State stateType) where
  pure = Pure
  (<*>) f s = Bind f (flip map s)

Monad (State stateType) where
  (>>=) = Bind

--mutual
--  Functor (State stateType) where
--    map f s = do
--      val <- s
--      pure (f val)
--
--  Applicative (State stateType) where
--    pure = Pure
--    (<*>) f s = do
--      f' <- f
--      map f' s
--
--  Monad (State stateType) where
--    (>>=) = Bind

runState : stateType -> State stateType a -> (stateType, a)
runState st Get = (st, st)
runState st (Put newState) = (newState, ())
runState st (Pure x) = (st, x)
runState st (Bind cmd prog) = let (nextState, val) = runState st cmd in
                                  runState nextState (prog val)

execState : stateType -> State stateType a -> stateType
execState st = fst . runState st

evalState : stateType -> State stateType a -> a
evalState st = snd . runState st

-------------------------- Add if positive

addIfPositive : Integer -> State Integer Bool
addIfPositive val = do
  let isValPositive = val > 0
  when isValPositive $ do
    current <- get
    put (current + val)
  pure isValPositive

addPositives : List Integer -> State Integer Nat
addPositives vals = do
  added <- traverse addIfPositive vals
  pure (length (filter id added))

