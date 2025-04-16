module ElemType

import Data.Vect
import Decidable.Equality

total
data Elem : (value : a) -> (xs : Vect k a) -> Type where
  Here : Elem value (value :: xs)
  There : Elem value xs -> Elem value (x :: xs)

total
notInNil : Elem value Nil -> Void
notInNil Here impossible
notInNil (There _) impossible

total
notInTail : (notThere : Elem value xs -> Void) ->
            (notHere : value = x -> Void) ->
            Elem value (x :: xs) -> Void
notInTail notThere notHere Here = notHere Refl
notInTail notThere notHere (There later) = notThere later

total
isElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
isElem value Nil = No notInNil
isElem value (x :: xs) = case decEq value x of
                              Yes Refl => Yes Here
                              No notHere => case isElem value xs of
                                                 Yes prf => Yes (There prf)
                                                 No notThere => No (notInTail notThere notHere)

total
elem' : Eq ty => (value : ty) -> (xs : Vect n ty) -> Bool
elem' value Nil = False
elem' value (x :: xs) = case value == x of
                             False => elem' value xs
                             True => True

