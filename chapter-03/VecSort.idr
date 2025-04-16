module VecSort

import Data.Vect

total
insSort : Ord e => Vect n e -> Vect n e
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insert x xsSorted
  where
    insert : e -> Vect m e -> Vect (S m) e
    insert x [] = [x]
    insert x (y :: ys) = case x < y of
                              False => y :: insert x ys
                              True => x :: y :: ys
