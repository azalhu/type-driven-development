module InfList

total
data InfList : Type -> Type where
  (::) : (value : elem') -> Inf (InfList elem') -> InfList elem'

total
countFrom : Integer -> InfList Integer
countFrom x = x :: countFrom (x + 1)

total
countN : Nat -> List Integer
countN n with (countFrom 0)
  countN Z | val = []
  countN (S n) | (val :: vals) = val :: countN n | vals

total
getPrefix : (count : Nat) -> InfList ty -> List ty
getPrefix Z xs = []
getPrefix (S k) (x :: xs) = x :: getPrefix k xs

total
Functor InfList where
  map f (x :: xs) = f x :: map f xs

