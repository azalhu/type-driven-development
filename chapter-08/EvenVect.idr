module EvenVect

import Data.Vect

total
data Parity : Type where
  PEven : Parity
  POdd : Parity

total
ParityType : Parity -> Type
ParityType PEven = Nat
ParityType POdd = Nat

total
data NatParity : Parity -> Type where
  Even : Nat -> NatParity parity
  Odd : Nat -> NatParity parity

total
fromNat : (k : Nat) -> Parity
fromNat k = if k `mod` 2 == 0
               then PEven
               else POdd

total
fromParity : (p : Parity) -> (k : Nat) -> NatParity p
fromParity PEven k = Even k
fromParity POdd k = Odd k

total
data EvenVect : (ParityType PEven) -> Type -> Type where


total
pairUp : Vect n ty

--fromNat : Nat -> Parity
--fromNat Z = Z
--fromNat (S k) = S (fromNat k)

--Even : Nat -> Type
--Even Z = Z

--pairUp : Vect (n * 2) ty -> Vect n (ty, ty)
--pairUp [] = []
--pairUp (x :: y :: rest) = (x, y) :: pairUp rest

