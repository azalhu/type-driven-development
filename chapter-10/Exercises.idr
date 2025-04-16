module Exercises

import Data.List.Views
import Data.Nat.Views
import DataStore
import Shape_abs

total
data TakeN : List a -> Type where
  Fewer : TakeN xs
  Exact : (n_xs : List a) -> {rest : _} -> TakeN (n_xs ++ rest)

total
takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN Z _ = Exact []
takeN (S n) [] = Fewer
takeN (S n) (x :: xs) with (takeN n xs)
  takeN (S n) (x :: xs) | Fewer = Fewer
  takeN (S n) (x :: (n_xs ++ rest)) | Exact n_xs = Exact (x :: n_xs)

covering
groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN Z xs = [xs]
groupByN n xs with (takeN n xs)
  groupByN n xs | Fewer = [xs]
  groupByN n (n_xs ++ rest) | Exact n_xs = n_xs :: groupByN n rest

total
halfLength : List a -> Nat
halfLength xs = integerToNat (div (natToInteger (length xs)) 2)

total
halves : List a -> (List a, List a)
halves xs with (takeN (halfLength xs) xs)
  halves xs | Fewer = (xs, [])
  halves (n_xs ++ rest) | Exact n_xs = (n_xs, rest)

covering
equalSuffix : Eq a => List a -> List a -> List a
equalSuffix left right with (snocList left, snocList right)
  equalSuffix _ _ | (Snoc x xs lrec, Snoc y ys rrec) =
    case x == y of
         False => []
         True => (equalSuffix xs ys | (lrec, rrec)) ++ [x]
  equalSuffix _ _ | (Empty, _) = []
  equalSuffix _ _ | (_, Empty) = []

total
toBinary : Nat -> String
toBinary n with (halfRec n)
  toBinary 0 | HalfRecZ = ""
  toBinary (n + n) | HalfRecEven n rec = (toBinary n | rec) ++ "0"
  toBinary (S (n + n)) | HalfRecOdd n rec = (toBinary n | rec) ++ "1"

total
getValues : DataStore (SString .+. val_schema) ->
            List (SchemaType val_schema)
getValues store with (storeView store)
  getValues DataStore.empty | SNil = []
  getValues (addToStore (key, value) store) | SAdd rec =
    value :: getValues store | rec

total
testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $
            addToStore ("Second", 2) $
            empty

total
area : Shape -> Double
area s with (shapeView s)
  area (triangle base height) | STriangle = 0.5 * base * height
  area (rectangle width height) | SRectangle = width * height
  area (circle radius) | SCircle = pi * radius * radius

