module ReverseVec

import Data.Vect

total
myReverse : Vect n elem' -> Vect n elem'
myReverse [] = []
myReverse (x :: xs) = reverseProof (myReverse xs ++ [x])
  where
    ||| prove (len + 1 : plus len 1) = (1 + len : S len)
    reverseProof : Vect (len + 1) elem' -> Vect (1 + len) elem'
    reverseProof xs = rewrite plusCommutative 1 len in xs

