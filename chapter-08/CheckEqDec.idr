module CheckEqDec

total
zeroNotSuc : Z = S j -> Void
zeroNotSuc Refl impossible

total
sucNotZero : S k = Z -> Void
sucNotZero Refl impossible

total
noRec : (contra : k = j -> Void) -> S k = S j -> Void
noRec contra Refl = contra Refl

total
checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat Z Z = Yes Refl
checkEqNat Z (S j) = No zeroNotSuc
checkEqNat (S k) Z = No sucNotZero
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Yes prf => Yes (cong S prf)
                              No contra => No (noRec contra)

