module Adder

import Data.Vect

total
AdderType : (numargs : Nat) -> Type -> Type
AdderType Z argType = argType
AdderType (S k) argType = (next : argType) -> AdderType k argType

total
adder : Num acc => (numargs : Nat) -> acc -> acc
adder Z acc = acc
adder (S numargs) acc = acc + adder numargs acc

total
adder' : Num acc => (numargs : Nat) -> acc -> Vect numargs acc -> acc
adder' Z acc _ = acc
adder' (S n) acc (x :: xs) = acc + adder' n x xs

total
adder'' : Num numType => (numargs : Nat) -> (acc : numType) -> AdderType numargs numType
adder'' Z acc = acc
adder'' (S k) acc = \next => adder'' k (next + acc)

