module TreeLabelType

%default total

-------------------------- State

data State : (stateType : Type) -> Type -> Type where
  Get : State stateType stateType
  Put : stateType -> State stateType ()
  ---------------
  Pure : a -> State stateType a
  Bind : State stateType a -> (a -> State stateType b) -> State stateType b

get : State stateType stateType
get = Get

put : stateType -> State stateType ()
put = Put

Functor (State stateType) where
  map f s = Bind s (Pure . f)

Applicative (State stateType) where
  pure = Pure
  (<*>) f s = Bind f (flip map s)

Monad (State stateType) where
  (>>=) = Bind

runState : stateType -> State stateType a -> (stateType, a)
runState st Get = (st, st)
runState st (Put newState) = (newState, ())
runState st (Pure x) = (st, x)
runState st (Bind cmd prog) = let (nextState, val) = runState st cmd in
                                  runState nextState (prog val)

execState : stateType -> State stateType a -> stateType
execState st = fst . runState st

evalState : stateType -> State stateType a -> a
evalState st = snd . runState st

-------------------------- Tree

data Tree a = Empty
            | Node (Tree a) a (Tree a)

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

-------------------------- Test

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

