module State

import Control.Monad.State

%default total

increase : Nat -> State Nat ()
increase inc = do
  current <- get
  put (current + inc)

increase' : (state : Nat) -> (inc : Nat) -> Nat
increase' state Z = state
increase' state (S k) = increase' (S state) k

