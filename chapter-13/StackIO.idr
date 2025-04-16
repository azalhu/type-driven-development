module StackIO

import Data.Vect

%default total

data StackCmd : Type -> Nat -> Nat -> Type where
  Push : Integer -> StackCmd () height (S height)
  Pop : StackCmd Integer (S height) height
  Top : StackCmd Integer (S height) (S height)
  ----------------
  GetStr : StackCmd String height height
  PutStr : String -> StackCmd () height height
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
           IO (ty, Vect outHeight Integer)
runStack stk (Push val) = pure ((), val :: stk)
runStack (val :: stk) Pop = pure (val, stk)
runStack (val :: stk) Top = pure (val, val :: stk)
----------------
runStack stk GetStr = getLine >>= \str => pure (str, stk)
runStack stk (PutStr str) = putStr str >> pure ((), stk)
----------------
runStack stk (Pure x) = pure (x, stk)
runStack stk (cmd >>= next) = do
  (cmdRes, newStk) <- runStack stk cmd
  runStack newStk (next cmdRes)
runStack stk (cmd >> next) = do
  ((), newStk) <- runStack stk cmd
  runStack newStk next

data StackIO : Nat -> Type where
  Do : StackCmd a height1 height2 ->
       (a -> Inf (StackIO height2)) ->
       StackIO height1
  Seq : StackCmd () height1 height2 ->
        Inf (StackIO height2) ->
        StackIO height1

namespace StackDo
  export
  (>>=) : StackCmd a height1 height2 ->
          (a -> Inf (StackIO height2)) ->
          StackIO height1
  (>>=) = Do

  export
  (>>) : StackCmd () height1 height2 ->
         Inf (StackIO height2) ->
         StackIO height1
  (>>) = Seq

data Fuel = Dry | More (Lazy Fuel)

covering
forever : Fuel
forever = More forever

run : Fuel -> Vect height Integer -> StackIO height -> IO ()
run Dry stk p = pure ()
run (More fuel) stk (Do cmd next) = do
  (res, newStk) <- runStack stk cmd
  run fuel newStk (next res)
run (More fuel) stk (Seq cmd next) = do
  ((), newStk) <- runStack stk cmd
  run fuel newStk next

data StkInput = Number Integer
              | Add
              | Subtract
              | Multiply
              | Negate
              | Discard
              | Duplicate

strToInput : String -> Maybe StkInput
strToInput "" = Nothing
strToInput "add" = Just Add
strToInput "subtract" = Just Subtract
strToInput "multiply" = Just Multiply
strToInput "negate" = Just Negate
strToInput "discard" = Just Discard
strToInput "duplicate" = Just Duplicate
strToInput x = if all isDigit (unpack x)
                  then Just (Number (cast x))
                  else Nothing

doAdd : StackCmd () (S (S height)) (S height)
doAdd = do
  val1 <- Pop
  val2 <- Pop
  Push (val2 + val1)

doSubtract : StackCmd () (S (S height)) (S height)
doSubtract = do
  val1 <- Pop
  val2 <- Pop
  Push (val2 - val1)

doMultiply : StackCmd () (S (S height)) (S height)
doMultiply = do
  val1 <- Pop
  val2 <- Pop
  Push (val2 * val1)

doNegate : StackCmd () (S height) (S height)
doNegate = do
  val <- Pop
  Push (-val)

doDiscard : StackCmd Integer (S height) height
doDiscard = Pop

doDuplicate : StackCmd () (S height) (S (S height))
doDuplicate = do
  val <- Top
  Push val

mutual
  tryAdd : {height : _} -> StackIO height
  tryAdd {height = S (S h)} = do
    doAdd
    result <- Top
    PutStr (show result ++ "\n")
    stackCalc
  tryAdd = do
    PutStr "Fewer than two items on the stack\n"
    stackCalc

  trySubtract : {height : _} -> StackIO height
  trySubtract {height = S (S h)} = do
    doSubtract
    result <- Top
    PutStr (show result ++ "\n")
    stackCalc
  trySubtract = do
    PutStr "Fewer than two items on the stack\n"
    stackCalc

  tryMultiply : {height : _} -> StackIO height
  tryMultiply {height = S (S h)} = do
    doMultiply
    result <- Top
    PutStr (show result ++ "\n")
    stackCalc
  tryMultiply = do
    PutStr "Fewer than two items on the stack\n"
    stackCalc

  tryNegate : {height : _} -> StackIO height
  tryNegate {height = S h} = do
    doNegate
    result <- Top
    PutStr (show result ++ "\n")
    stackCalc
  tryNegate = do
    PutStr "No items on the stack\n"
    stackCalc

  tryDiscard : {height : _} -> StackIO height
  tryDiscard {height = S h} = do
    result <- doDiscard
    PutStr ("Discarded " ++ show result ++ "\n")
    stackCalc
  tryDiscard = do
    PutStr "No items on the stack\n"
    stackCalc

  tryDuplicate : {height : _} -> StackIO height
  tryDuplicate {height = S h} = do
    doDuplicate
    result <- Top
    PutStr ("Duplicated " ++ show result ++ "\n")
    stackCalc
  tryDuplicate = do
    PutStr "No items on the stack\n"
    stackCalc

  stackCalc : {height : _} -> StackIO height
  stackCalc = do
    PutStr "> "
    input <- GetStr
    case strToInput input of
         Nothing => do
           PutStr "Invalid input\n"
           stackCalc
         Just (Number x) => do
           Push x
           stackCalc
         Just Add => tryAdd
         Just Subtract => trySubtract
         Just Multiply => tryMultiply
         Just Negate => tryNegate
         Just Discard => tryDiscard
         Just Duplicate => tryDuplicate

covering
main : IO ()
main = run forever [] stackCalc

