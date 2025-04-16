module SnocList

import Data.List

total
data MySnocList' ty = Empty' | Snoc' (MySnocList' ty) ty

total
reverseSnoc' : MySnocList' a -> List a
reverseSnoc' Empty' = []
reverseSnoc' (Snoc' xs x) = x :: reverseSnoc' xs

total
data MySnocList : List ty -> Type where
  Empty : MySnocList []
  Snoc : {x, xs : _} ->
         (rec : MySnocList xs) ->
         MySnocList (xs ++ [x])

total
snocList : (input : List a) -> MySnocList input
snocList input = snocListHelp Empty input
  where
    snocListHelp : {input' : _} ->
                   (snoc : MySnocList input') ->
                   (rest : List a) ->
                   MySnocList (input' ++ rest)
    snocListHelp snoc [] = rewrite appendNilRightNeutral input' in snoc
    snocListHelp snoc (x :: xs) =
      rewrite appendAssociative input' [x] xs in snocListHelp (Snoc snoc) xs

total
myReverse : List a -> List a
myReverse input with (snocList input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | Snoc rec = x :: myReverse xs | rec

