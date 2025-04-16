module Fibs

total
fibs_iter : (n : Nat) -> (m : Nat) -> (acc : Nat) -> Nat
fibs_iter Z _ _ = Z
fibs_iter (S Z) _  _ = (S Z)
fibs_iter n m acc = ?rh

total
fibs : Nat -> Nat
fibs n = fibs_iter n 0 0

total
fibs_iter' : (n : Nat) -> (m : Nat) -> (acc : Nat) -> Nat
fibs_iter' Z _ _ = Z
fibs_iter' (S Z) _  _ = (S Z)
fibs_iter' n m acc = ?rh'

total
fibs' : Nat -> Nat
fibs' (S (S n)) = fibs' (S n) + fibs' n
fibs' n = n

