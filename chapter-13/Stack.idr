module Stack

import Data.Vect

%default total

data StackCmd : Type -> Nat -> Nat -> Type where
  Push : Integer -> StackCmd () height (S height)
  Pop : StackCmd Integer (S height) height
  Top : StackCmd Integer (S height) (S height)
  ----------------
  Pure : ty -> StackCmd ty height height
  (>>=) : StackCmd a height1 height2 ->
          (a -> StackCmd b height2 height3) ->
          StackCmd b height1 height3
  (>>) : StackCmd () height1 height2 ->
         Lazy (StackCmd b height2 height3) ->
         StackCmd b height1 height3

runStack : (stk : Vect inHeight Integer) ->
           StackCmd ty inHeight outHeight ->
           (ty, Vect outHeight Integer)
runStack stk (Push val) = ((), val :: stk)
runStack (val :: stk) Pop = (val, stk)
runStack (val :: stk) Top = (val, val :: stk)

runStack stk (Pure x) = (x, stk)
runStack stk (cmd >>= next) =
  let (cmdRes, newStk) = runStack stk cmd in
      runStack newStk (next cmdRes)
runStack stk (cmd >> next) =
  let ((), newStk) = runStack stk cmd in
      runStack newStk next

testAdd : StackCmd Integer Z Z
testAdd = do
  Push 10
  Push 20
  val1 <- Pop
  val2 <- Pop
  Pure (val1 + val2)

doAdd : StackCmd () (S (S height)) (S height)
doAdd = do
  val1 <- Pop
  val2 <- Pop
  Push (val1 + val2)

