module Void

total
twoPlusTwoNotFive : 2 + 2 = 5 -> Void
twoPlusTwoNotFive Refl impossible

total
valueNotSuc : (x : Nat) -> x = S x -> Void
valueNotSuc _ Refl impossible

