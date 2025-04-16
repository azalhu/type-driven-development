module Exercises

import Data.Nat
import Data.Vect
import Decidable.Equality

total
same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons Refl = Refl

total
same_lists : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl Refl = Refl

total
data ThreeEq : a -> b -> c -> Type where
  AllSame : ThreeEq x x x

total
allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS _ _ _ AllSame = AllSame

total
myPlusCommutative : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutative Z m = sym (plusZeroRightNeutral m)
myPlusCommutative (S k) m = rewrite myPlusCommutative k m in plusSuccRightSucc m k

total
myPlusCommutative' : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutative' Z m = rewrite plusZeroRightNeutral m in Refl
myPlusCommutative' (S k) m = rewrite myPlusCommutative k m in
                             rewrite plusSuccRightSucc m k in Refl

total
myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where
    reverseProof_nil : Vect k a -> Vect (plus k 0) a
    reverseProof_xs : Vect (S (plus k len)) a -> Vect (plus k (S len)) a

    reverse' : Vect k a -> Vect m a -> Vect (k + m) a
    reverse' acc [] = reverseProof_nil acc
    reverse' acc (x :: xs) = reverseProof_xs (reverse' (x :: acc) xs)

    reverseProof_nil xs = rewrite plusZeroRightNeutral k in xs
    reverseProof_xs xs = rewrite sym (plusSuccRightSucc k len) in xs

total
data MyVect : Nat -> Type -> Type where
  Nil : MyVect Z a
  (::) : a -> MyVect k a -> MyVect (S k) a

total
headUnequal : DecEq a =>
              {xs : MyVect n a} ->
              {ys : MyVect n a} ->
              (contra : x = y -> Void) ->
              (x :: xs) = (y :: ys) -> Void
headUnequal contra Refl = contra Refl

total
tailUnequal : DecEq a =>
              {xs : MyVect n a} ->
              {ys : MyVect n a} ->
              (contra : xs = ys -> Void) ->
              (x :: xs) = (y :: ys) -> Void
tailUnequal contra Refl = contra Refl

total
DecEq a => DecEq (MyVect n a) where
  decEq Nil Nil = Yes Refl
  decEq (x :: xs) (y :: ys) = case decEq xs ys of
                                   No contra => No (tailUnequal contra)
                                   Yes Refl => case decEq x y of
                                                    No contra => No (headUnequal contra)
                                                    Yes Refl => Yes Refl

