module Exercises

import Decidable.Equality

total
data ListElem : (value : a) -> (xs : List a) -> Type where
  ListHere : ListElem value (value :: xs)
  ListThere : ListElem value xs -> ListElem value (y :: xs)

total
data Last : (xs : List a) -> (value : a) -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

total
last123 : Last [1, 2, 3] 3
last123 = LastCons (LastCons LastOne)

total
last1 : Last xs a

total
notInNil : Last Nil value -> Void
notInNil _ impossible

total
notInSingleton : (contra : value = x -> Void) ->
                 Last (x :: Nil) value -> Void
notInSingleton contra LastOne = contra Refl

total
notInTail : (contra : Last (x :: xs) value -> Void) ->
            Last (y :: x :: xs) value -> Void
notInTail contra (LastCons prf) = contra prf

total
isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast Nil value = No notInNil
isLast (x :: Nil) value = case decEq value x of
                               Yes Refl => Yes LastOne
                               No contra => No (notInSingleton contra)
isLast (y :: x :: xs) value = case isLast (x :: xs) value of
                                   Yes prf => Yes (LastCons prf)
                                   No contra => No (notInTail contra)

