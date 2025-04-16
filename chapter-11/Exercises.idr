module Exercises

import Arith
import Data.Primitives.Views
import Data.Stream

total
every_other : Stream a -> Stream a
every_other (y :: x :: xs) = x :: every_other xs

total
data Face = Heads | Tails

total
coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips count nums = map getFace (take count nums)
  where
    getFace : Int -> Face
    getFace n with (divides n 2)
      getFace ((2 * div) + rem) | DivBy div rem prf =
        case rem of
             0 => Heads
             _ => Tails

    getFace' : Int -> Face
    getFace' n = if n `mod` 2 == 0 then Heads else Tails

total
square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = iterate next approx
  where
    next : Double -> Double
    next approx = (approx + (number / approx)) / 2

total
square_root_bound : (max : Nat) ->
                    (number : Double) ->
                    (bound : Double) ->
                    (approxs : Stream Double) ->
                    Double
square_root_bound Z _ _ (val :: vals) = val
square_root_bound (S max) number bound (val :: vals) =
  if (abs val * val - number < bound)
     then val
     else square_root_bound max number bound vals

total
square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001 (square_root_approx number number)

