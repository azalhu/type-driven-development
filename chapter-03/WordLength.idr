module WordLength

import Data.Vect

total
allLengths : List String -> List Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

total
allLengthsSnoc : SnocList String -> SnocList Nat
allLengthsSnoc [<] = [<]
allLengthsSnoc (words :< word) = allLengthsSnoc words :< length word

total
allLengthsVect : Vect k String -> Vect k Nat
allLengthsVect [] = []
allLengthsVect (word :: words) = length word :: allLengthsVect words
