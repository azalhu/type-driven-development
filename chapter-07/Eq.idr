module Eq

total
occurrences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurrences _ [] = Z
occurrences item (x :: xs) = acc (item == x) (occurrences item xs)
  where
    acc : Bool -> Nat -> Nat
    acc False n = n
    acc True n = S n

total
data Matter = Solid | Liquid | Gas

total
Eq Matter where
  (==) Solid Solid = True
  (==) Liquid Liquid = True
  (==) Gas Gas = True
  (==) _ _ = False

total
data Tree e = Empty
            | Node (Tree e) e (Tree e)

total
insert : Ord e => e -> Tree e -> Tree e
insert e Empty = Node Empty e Empty
insert e orig@(Node left val right) = case compare e val of
                                      LT => Node (insert e left) val right
                                      EQ => orig
                                      GT => Node left val (insert e right)

total
Eq e => Eq (Tree e) where
  (==) Empty Empty = True
  (==) (Node left e right) (Node left' e' right') = left == left' && e == e' && right == right'
  (==) _ _ = False

