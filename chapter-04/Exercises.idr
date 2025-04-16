module Exercises

import Data.Vect
import DataTypes

total
listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

total
treeToList : Ord a => Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ val :: treeToList right

--listToTree : Ord a => List a -> Tree a
--listToTree list = insertList list Empty
--  where insertList : List a -> Tree a -> Tree a
--        insertList [] tree = tree
--        insertList (x :: xs) tree = insertList xs (insert x tree)

--treeToList : Ord a => Tree a -> List a
--treeToList tree = insertTree tree []
--  where
--    insertTree : Tree a -> List a -> List a
--    insertTree Empty list = list
--    insertTree (Node left val right) list = (insertTree left list)
--                                            ++ (val :: list)
--                                            ++ (insertTree right list)

--data Expr : Type -> Type where
--  Single : Num a => a -> Expr a
--  Addition : Num a => Expr a -> Expr a -> Expr a
--  Subtraction : Num a => Expr a -> Expr a -> Expr a
--  Multiplication : Num a => Expr a -> Expr a -> Expr a

data Expr = Single Int
          | Addition Expr Expr
          | Subtraction Expr Expr
          | Multiplication Expr Expr

evaluate : Expr -> Int
evaluate (Single x) = x
evaluate (Addition x y) = evaluate x + evaluate y
evaluate (Subtraction x y) = evaluate x - evaluate y
evaluate (Multiplication x y) = evaluate x * evaluate y

total
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe x Nothing = x
maxMaybe (Just x) (Just y) = Just (max x y)

total
biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive tri@(Triangle _ _)) = Just (area tri)
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine pic1 pic2) = maxMaybe (biggestTriangle pic1) (biggestTriangle pic2)
biggestTriangle (Rotate _ pic) = biggestTriangle pic
biggestTriangle (Translate _ _ pic) = biggestTriangle pic

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

total
vectTake : (n : Nat) -> Vect (n + m) a -> Vect n a
vectTake Z xs = []
vectTake (S n) (x :: xs) = x :: vectTake n xs

total
sumEntries : Num a => {n : _} -> (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries pos xs ys = case integerToFin pos n of
                            Nothing => Nothing
                            Just posFin => Just (index posFin xs + index posFin ys)
