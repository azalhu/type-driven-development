module Main

doubleInt : Int -> Int
doubleInt x = x + x

doubleNat : Nat -> Nat
doubleNat x = x + x

doubleInteger : Integer -> Integer
doubleInteger x = x + x

double : Num ty => ty -> ty
double x = x + x

quadruple : Num a => a -> a
quadruple x = double (double x)
