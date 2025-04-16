module AppendVec

import Data.Vect

total
append_nil : Vect m elem' -> Vect (plus m 0) elem'
append_nil xs = rewrite plusZeroRightNeutral m in xs

total
append_xs : Vect (S (plus m len)) elem' -> Vect (plus m (S len)) elem'
append_xs xs = rewrite sym (plusSuccRightSucc m len) in xs

total
append : Vect n elem' -> Vect m elem' -> Vect (m + n) elem'
append [] ys = append_nil ys
append (x :: xs) ys = append_xs (x :: append xs ys)

