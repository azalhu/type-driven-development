module TreeLabelState

import Control.Monad.State

%default total

public export
data Tree a = Empty
            | Node (Tree a) a (Tree a)

export
testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

export
flatten : Tree a -> List a
flatten Empty = []
flatten (Node left val right) = flatten left ++ val :: flatten right

treeLabelWith : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWith Empty = pure Empty
treeLabelWith (Node left val right) = do
  left_labelled <- treeLabelWith left
  (this :: rest) <- get
  put rest
  right_labelled <- treeLabelWith right
  pure (Node left_labelled (this, val) right_labelled)

treeLabel : Tree a -> Tree (Integer, a)
treeLabel = evalState [1..] . treeLabelWith


------------------------------------------------------- Just for fun


treeLabelWith' : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWith' Empty = pure Empty
treeLabelWith' (Node left val right) =
  treeLabelWith' left >>=
    \left_labelled => get >>=
      \(this :: rest) => put rest >>=
        \() => treeLabelWith' right >>=
          \right_labelled => pure (Node left_labelled (this, val) right_labelled)

treeLabelTest : Tree a -> List (Integer, a)
treeLabelTest = flatten . evalState [1..] . treeLabelWith'

