module Exercises

import Control.Monad.State
import TreeLabelState

%default total


-------------------- Count Empty


update : (stateType -> stateType) -> State stateType ()
update f = do
  state <- get
  put (f state)

countEmpty : Tree a -> State Nat ()
countEmpty Empty = update S
countEmpty (Node left _ right) = do
  countEmpty left
  countEmpty right


-------------------- Count Empty Node


updateFst : (a -> a) -> State (a, b) ()
updateFst f = do
  (a, b) <- get
  put (f a, b)

updateSnd : (b -> b) -> State (a, b) ()
updateSnd f = do
  (a, b) <- get
  put (a, f b)

countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = updateFst S
countEmptyNode (Node left _ right) = do
  countEmptyNode left
  updateSnd S
  countEmptyNode right


-------------------- Increase


increase : Nat -> State Nat ()
increase = update . (+)

increaseTest : Bool
increaseTest = 104 == execState 45 (increase 59)

countEmptyNodeTest : (Nat, Nat)
countEmptyNodeTest = execState (0, 0) (countEmptyNode testTree)


-------------------- Social News Website


record Votes where
  constructor MkVotes
  upvotes : Integer
  downvotes : Integer

record Article where
  constructor MkArticle
  title : String
  url : String
  score : Votes

initPage : (title : String) -> (url : String) -> Article
initPage title url = MkArticle title url (MkVotes 0 0)

badSite : Article
badSite = MkArticle "Bad Page" "http://example.com/bad" (MkVotes 5 47)

goodSite : Article
goodSite = MkArticle "Good Page" "http://example.com/good" (MkVotes 101 7)

getScore : Article -> Integer
getScore article = article.score.upvotes - article.score.downvotes

addUpvote : Article -> Article
addUpvote = { score->upvotes $= (+1) }

addDownvote : Article -> Article
addDownvote = { score->downvotes $= (+1) }

