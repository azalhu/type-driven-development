module Exercises

import Data.Vect

total
Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Double)

total
testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

total
testMatrix2 : Matrix 0 2
testMatrix2 = []

total
testMatrix3 : Matrix 2 0
testMatrix3 = [[], []]

total
TupleVect : Nat -> Type -> Type
TupleVect Z _ = ()
TupleVect (S k) ty = (ty, (TupleVect k ty))

total
test : TupleVect 4 Nat
test = (1, 2, 3, 4, ())
