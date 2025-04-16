module Exercises

import Data.Vect

total
my_length : List a -> Nat
my_length [] = Z
my_length (x :: xs) = S (my_length xs)

total
my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_reverse xs ++ [x]

namespace MyList
  total
  export
  my_map : (a -> b) -> List a -> List b
  my_map f [] = []
  my_map f (x :: xs) = f x :: my_map f xs

namespace MyVect
  total
  export
  my_map : (a -> b) -> Vect n a -> Vect n b
  my_map f [] = []
  my_map f (x :: xs) = f x :: my_map f xs

total
transposeMat : {n : _} -> Vect m (Vect n e) -> Vect n (Vect m e)
transposeMat [] = replicate n []
transposeMat (x :: xs) = zipWith (::) x (transposeMat xs)

total
addMatrix : Num a => Vect m (Vect n a) -> Vect m (Vect n a) -> Vect m (Vect n a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

total
zipMultVect : Num a => Vect m a -> Vect m a -> a
zipMultVect xs ys = sum (zipWith (*) xs ys)

total
multVectHelper : Num a => Vect n a -> Vect p (Vect n a) -> Vect p a
multVectHelper _ [] = []
multVectHelper x (y :: ys) = zipMultVect x y :: multVectHelper x ys

total
multMatHelper : Num a => Vect m (Vect n a) -> Vect p (Vect n a) -> Vect m (Vect p a)
multMatHelper [] _ = []
multMatHelper (x :: xs) ys = multVectHelper x ys :: multMatHelper xs ys

total
multMatrix : Num a => {p : _} -> Vect m (Vect n a) -> Vect n (Vect p a) -> Vect m (Vect p a)
multMatrix xs ys = let ysTransposed = transposeMat ys in
                       multMatHelper xs ysTransposed

--total
--multHelper : Num a => Vect n a -> Vect p (Vect n a) -> Vect p a
--multHelper _ [] = []
--multHelper x (y :: ys) = sum (zipWith (*) x y) :: multHelper x ys
--
--total
--multMatrix : Num a => {p : _} -> Vect m (Vect n a) -> Vect n (Vect p a) -> Vect m (Vect p a)
--multMatrix [] _ = []
--multMatrix (x :: xs) ys = let ysTransposed = transposeMat ys in
--                              multHelper x ysTransposed :: multMatrix xs ys
