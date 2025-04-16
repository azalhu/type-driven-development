module ExactLengthDec

import Decidable.Equality

total
data Vect : Nat -> Type -> Type where
  Nil : Vect Z _
  (::) : a -> Vect k a -> Vect (S k) a

total
exactLength : {m : _} -> (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength len input = case decEq m len of
                             Yes Refl => Just input
                             No contra => Nothing

