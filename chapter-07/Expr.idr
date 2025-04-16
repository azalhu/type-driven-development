module Expr

total
data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

total
Eval : Type -> Type
Eval num = (Neg num, Integral num, Abs num)

total
eval : Eval num => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

total
Num num => Num (Expr num) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

total
Neg num => Neg (Expr num) where
  negate = Sub (Val (fromInteger 0))
  (-) = Sub

total
Abs num => Abs (Expr num) where
  abs = Abs

total
Integral Double where
  div x y = the Double (cast (the Integer ((cast x) `div` (cast y))))
  mod x y = the Double (cast (the Integer ((cast x) `mod` (cast y))))

total
Cast (Maybe e) (List e) where
  cast Nothing = Nil
  cast (Just x) = x :: Nil

total
Show num => Show (Expr num) where
  show (Val x) = show x
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " `div` " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Abs x) = show x

total
EqEval : Type -> Type
EqEval num = (Eq num, Eval num)

total
EqEval num => Eq (Expr num) where
  (==) x y = eval x == eval y

total
CastEval : Type -> Type -> Type
CastEval fromNum toNum = (Cast fromNum toNum, Eval fromNum)

total
CastEval fromNum toNum => Cast (Expr fromNum) toNum where
  cast x = cast (eval x)

total
Functor Expr where
  map f (Val x) = Val (f x)
  map f (Add x y) = Add (map f x) (map f y)
  map f (Sub x y) = Sub (map f x) (map f y)
  map f (Mul x y) = Mul (map f x) (map f y)
  map f (Div x y) = Div (map f x) (map f y)
  map f (Abs x) = Abs (map f x)

--total
--data Expr num = Val num
--              | Add (Expr num) (Expr num)
--              | Sub (Expr num) (Expr num)
--              | Mul (Expr num) (Expr num)
--              | Div (Expr num) (Expr num)
--              | Abs (Expr num)

--record MyFunctor where
--  constructor MkMyFunctor
--  f : (Type -> Type)
--
--total
--bob' : Maybe (Integer, Integer)
--bob' = Just (MkPair 3) <*> Just 6
--
--total
--bob : Maybe (Integer, Integer)
--bob = Just swap <*> bob'

