module Exercises

import Data.String

palindrome : Nat -> String -> Bool
palindrome n str = palinCaseIns str && isLongerThan n str
  where
    palinCaseIns : String -> Bool
    palinCaseIns str = let str = toLower str in
                           str == reverse str

    isLongerThan : Nat -> String -> Bool
    isLongerThan n str = length str > n

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

top_ten : Ord a => List a -> List a
top_ten elems = take 10 (reverse (sort elems))

over_length : Nat -> List String -> Nat
over_length n strs = let lengths = map length strs in
                         count (> n) lengths
