module ListExercises

import Data.List.Elem
import Decidable.Equality

%default total

elem' : Eq a => a -> List a -> Bool
elem' _ Nil = False
elem' y (x :: xs) = y == x || elem' y xs

elem'' : DecEq a => (y : a) -> (xs : List a) -> Dec (Elem y xs)
elem'' y Nil = No (\e => case e of
                  Here impossible
                  There _ impossible)
elem'' y (x :: xs) = case decEq y x of
                          Yes Refl => Yes Here
                          No notHere => case elem'' y xs of
                                             Yes prf => Yes (There prf)
                                             No notThere => No (notInTail notThere notHere)
  where
    notInTail : (notThere : Elem value zs -> Void) ->
                (notHere : value = z -> Void) ->
                Elem value (z :: zs) -> Void
    notInTail notThere notHere Here = notHere Refl
    notInTail notThere notHere (There later) = notThere later

