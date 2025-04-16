module Fold

total
totalLen : List String -> Nat
totalLen xs = foldr (\e, acc => length e + acc) Z xs

