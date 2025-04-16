module Exercises

total
data Shape : Type where
  Triangle : (base : Double) -> (height : Double) -> Shape
  Rectangle : (length : Double) -> (height : Double) -> Shape
  Circle : (radius : Double) -> Shape

total
area : Shape -> Double
area (Triangle base height) = base * height / 2
area (Rectangle length height) = length * height
area (Circle radius) = pi * pow radius 2

total
Eq Shape where
  (==) (Triangle base height) (Triangle base' height') = base == base' && height == height'
  (==) (Rectangle length height) (Rectangle length' height') = length == length' && height == height'
  (==) (Circle radius) (Circle radius') = radius == radius'
  (==) _ _ = False

total
Ord Shape where
  compare s1 s2 = compare (area s1) (area s2)

total
testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]

total
data MyVect : Nat -> Type -> Type where
  Nil : MyVect Z a
  (::) : (x : a) -> (xs : MyVect k a) -> MyVect (S k) a

Eq ty => Eq (MyVect n ty) where
  Nil == Nil = True
  (x :: xs) == (y :: ys) = x == y && xs == ys

Foldable (MyVect n) where
  foldr _ acc Nil = acc
  foldr func acc (elem :: elems) = func elem (foldr func acc elems)

Functor (MyVect n) where
  map _ Nil = Nil
  map func (elem :: elems) = func elem :: map func elems

