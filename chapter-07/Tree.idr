module Tree

total
data Tree elem' = Empty
                | Node (Tree elem') elem' (Tree elem')

total
leaf : elem' -> Tree elem'
leaf elem' = Node Empty elem' Empty

total
Functor Tree where
  map func Empty = Empty
  map func (Node left elem right) = Node (map func left) (func elem) (map func right)

total
Foldable Tree where
  foldr _ acc Empty = acc
  foldr func acc (Node left elem right) = do
    let acc_right = foldr func acc right
    let acc_left = foldr func acc_right left
    func elem acc_left

total
test : Tree Integer
test = Node (Node (leaf 1) 2 (leaf 3)) 4 (Node (leaf 5) 6 (leaf 7))
--test = Node (Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)

-- One-liner foldr
--
--total
--Foldable Tree where
--  foldr _ acc Empty = acc
--  foldr func acc (Node left elem right) = func elem (foldr func (foldr func acc right) left)

-- Non-total (recursively calls (foldl -> foldr))
--
--covering
--Foldable Tree where
--  foldr func acc = foldl (flip func) acc

