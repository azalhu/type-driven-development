module ExactLength

import EqNat

total
data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

total
exactLength : {m : _} -> (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength len input = do
  Refl <- checkEqNat len m
  Just input

