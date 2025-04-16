module CorrectByConstruction

%default total

data Region : Type

data Owned : (region : Region) ->
             (start, end : Nat) ->
             (vs : List Bits8) ->
             Type where
