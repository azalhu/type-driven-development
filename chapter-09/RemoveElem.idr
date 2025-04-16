module RemoveElem

import Data.Vect
import Data.Vect.Elem
import Decidable.Equality

total
oneInVector : Elem 1 [1, 2, 3]
oneInVector = Here

total
maryInVector : Elem "Mary" ["Peter", "Paul", "Mary"]
maryInVector = There (There Here)

total
fourNotInVector : Elem 4 [1, 2, 3] -> Void

total
peteNotInVector : Elem "Pete" ["John", "Paul", "George", "Ringo"] -> Void

export
total
removeElem' : {n : _} ->
              (value : a) ->
              (xs : Vect (S n) a) ->
              (prf : Elem value xs) ->
              Vect n a
removeElem' value (value :: ys) Here = ys
removeElem' {n = Z} value (y :: Nil) (There later) = absurd later -- optional clause / impossible
removeElem' {n = S k} value (y :: ys) (There later) = y :: removeElem' value ys later

total
removeElem_auto : {n : _} ->
                  (value : a) ->
                  (xs : Vect (S n) a) ->
                  {auto prf : Elem value xs} ->
                  Vect n a
removeElem_auto value xs = removeElem' value xs prf

export
total
removeElem : {n : _} ->
             (value : a) ->
             (xs : Vect (S n) a) ->
             {auto prf : Elem value xs} ->
             Vect n a
removeElem value (value :: ys) {prf = Here} = ys
removeElem {n = Z} value (y :: Nil) {prf = There later} = absurd later
removeElem {n = S k} value (y :: ys) {prf = There later} = y :: removeElem value ys

