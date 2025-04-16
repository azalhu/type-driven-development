module EqNat

export
total
checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat Z Z = Just Refl
checkEqNat (S k) (S j) = do
   prf <- checkEqNat k j
   Just (cong S prf)
checkEqNat _ _ = Nothing

