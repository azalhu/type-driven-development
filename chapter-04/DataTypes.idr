module DataTypes

import Data.Vect

-- Enumerated types

total
data Direction = North | East | South | West

total
turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North

-- Union types

-- -- Verbose `data` declaration

{-
||| Represents shapes
data Shape : Type where
  ||| A triangle, with its base length and height
  Triangle : Double -> Double -> Shape
  ||| A rectangle, with its length and height
  Rectangle : Double -> Double -> Shape
  ||| A circle, with its radius
  Circle : Double -> Shape
-}

-- -- Concise `data` declaration

||| Represents shapes
total
public export
data Shape = ||| A triangle, with its base length and height
             Triangle Double Double
           | ||| A rectangle, with its length and height
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

total
public export
area : Shape -> Double
area (Triangle base height) = base * height / 2
area (Rectangle length height) = length * height
area (Circle radius) = pi * pow radius 2

-- Recursive types

total
public export
data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

total
rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

total
circle : Picture
circle = Primitive (Circle 5)

total
triangle : Picture
triangle = Primitive (Triangle 10 10)

total
testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
              (Combine (Translate 35 5 circle)
              (Translate 15 25 triangle))

total
public export
pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine picture1 picture2) = pictureArea picture1 + pictureArea picture2
pictureArea (Rotate angle picture) = pictureArea picture
pictureArea (Translate x y picture) = pictureArea picture

-- Generic types

-- -- `Maybe` is generic

baggestCombinedTriangle : Maybe Double -> Maybe Double -> Maybe Double
baggestCombinedTriangle Nothing Nothing = Nothing
baggestCombinedTriangle area1 Nothing = area1
baggestCombinedTriangle Nothing area2 = area2
baggestCombinedTriangle (Just area1) (Just area2) = if area1 > area2
                                                       then Just area1
                                                       else Just area2

public export
baggestTriangle : Picture -> Maybe Double
baggestTriangle (Primitive shape) = case shape of
                                         Triangle _ _ => Just (area shape)
                                         _ => Nothing
baggestTriangle (Combine picture1 picture2) = let baggest1 = baggestTriangle picture1
                                                  baggest2 = baggestTriangle picture2 in
                                                  baggestCombinedTriangle baggest1 baggest2
baggestTriangle (Rotate angle picture) = baggestTriangle picture
baggestTriangle (Translate x y picture) = baggestTriangle picture

namespace Tree
  public export
  data Tree e = Empty
              | Node (Tree e) e (Tree e)

  export
  insert : Ord e => e -> Tree e -> Tree e
  insert e Empty = Node Empty e Empty
  insert e orig@(Node left val right) = case compare e val of
                                        LT => Node (insert e left) val right
                                        EQ => orig
                                        GT => Node left val (insert e right)

namespace BSTree
  data BSTree : Type -> Type where
    Empty : Ord e => BSTree e
    Node : Ord e => BSTree e -> e -> BSTree e -> BSTree e

  insert : e -> BSTree e -> BSTree e
  insert e Empty = Node Empty e Empty
  insert e orig@(Node left val right) = case compare e val of
                                             LT => Node (insert e left) val right
                                             EQ => orig
                                             GT => Node left val (insert e right)

-- Dependent types

total
data PowerSource = Electricity | Petrol | Pedal

total
data Vehicle : PowerSource -> Type where
  Unicycle : Vehicle Pedal
  Bicycle : Vehicle Pedal
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Tram : (electricity : Nat) -> Vehicle Electricity
  ElectricCar : (electricity : Nat) -> Vehicle Electricity

total
wheels : Vehicle powerSource -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Motorcycle fuel) = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (Tram electricity) = 4
wheels (ElectricCar electricity) = 4

total
refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 400

total
data MyVect : Nat -> Type -> Type where
  Nil : MyVect Z a
  (::) : (x : a) -> (xs : MyVect k a) -> MyVect (S k) a

total
vec1 : MyVect 3 Int
vec1 = [1, 2, 3]

total
vec2 : MyVect 2 Int
vec2 = [4, 5]

total
append : MyVect n e -> MyVect m e -> MyVect (n + m) e
append [] ys = ys
append (x :: xs) ys = x :: (append xs ys)

total
vec3 : MyVect 4 Int
vec3 = [1, 2, 3, 4]

total
vec4 : MyVect 4 String
vec4 = ["one", "two", "three", "four"]

total
zip : MyVect n a -> MyVect n b -> MyVect n (a, b)
zip [] ys = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys

total
tryIndex : {n : _} -> Integer -> Vect n a -> Maybe a
tryIndex i xs = case integerToFin i n of
                     Nothing => Nothing
                     Just iFin => Just (index iFin xs)

--total
--data Fin : Nat -> Type where
--  FZ : Fin (S k)
--  FS : Fin k -> Fin (S k)
--
--total
--ntf : Nat -> (n : Nat) -> Maybe (Fin n)
--ntf j 0 = Nothing
--ntf j m = if j < m
--             then Just ?fdsf
--             else Nothing
--
--natToFinLTE : (x : Nat) -> {auto 0 _ : LTE (S x) n} -> Fin m
--natToFinLTE Z = FZ
--natToFinLTE (S k) = FS (natToFinLTE k)
--
--total
--integerToFin : Integer -> (n : Nat) -> Maybe (Fin n)
--integerToFin i n = if i >= 0 then natToFin (fromInteger i) n else Nothing
--  where
--    natToFinLT : Nat -> Fin m
--    natToFinLT Z = FZ
--    natToFinLT (S k) = FS natToFinLT k
--
--    natToFin : Nat -> (m : Nat) -> Maybe (Fin m)
--    natToFin j 0 = Nothing
--    natToFin j m = if j < m then Just (natToFinLT j) else Nothing

--total
--index : Fin n -> Vect n a -> a
--index
