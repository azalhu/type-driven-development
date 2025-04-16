module DecFunctor

total
notBEq : (a = b -> c = d) -> Not (a = b) -> Not (c = d)
notBEq x y Refl = ?rhs

total
notB : (a -> b) -> Not a -> Not b
notB f x = ?tt

total
never : (a = b -> k = j) -> (a = b -> Void) -> k = j -> Void
never f x = ?nev

total
never' : (a -> b) -> (a -> Void) -> b -> Void
never' f x = ?nev'

total
data DecFn : Type -> Type where
  MkDecFn : (a = b) -> DecFn (Dec a = b)

total
Functor Dec where
  map func (No contra) = Yes ?mm
  map func (Yes prf) = Yes (func prf)

total
Functor (Either (Dec a)) where
  map = ?rhg

--total
--Monad Dec where
--  No c >>= _ = No c
--  Yes p >>= f = f p

