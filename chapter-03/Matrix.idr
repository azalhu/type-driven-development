module Matrix

import Data.Vect

total
transposeVec0 : Vect 0 (Vect 0 e) -> Vect 0 (Vect 0 e)
transposeVec0 [] = []

total
transposeVecX : Vect 1 (Vect n e) -> Vect n (Vect 1 e)
transposeVecX [[]] = []
transposeVecX [y :: ys] = let ysTransposed = transposeVecX [ys] in
                              [y] :: ysTransposed

total
transposeVecY : Vect n (Vect 1 e) -> Vect 1 (Vect n e)
transposeVecY [] = [[]]
transposeVecY ([y] :: xs) = let xsTransposed = head (transposeVecY xs) in
                                [y :: xsTransposed]

--total
--transposeMat : Vect n (Vect m e) -> Vect m (Vect n e)
--transposeMat [] = ?mata
--transposeMat [[]] = []
--transposeMat [y :: ys] = let ysTransposed = transposeMat [ys] in
--                             [y] :: ysTransposed
--transposeMat (ys :: xs) = let ysTransposed = transposeMat [ys]
--                              xsTransposed = transposeMat xs in
--                              zipWith (++) ysTransposed xsTransposed

total
createEmpties : {n : _} -> Vect n (Vect 0 e)
createEmpties = replicate n []

total
transposeHelper : Vect n e -> Vect n (Vect m e) -> Vect n (Vect (S m) e)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

total
transposeMat : {n : _} -> Vect m (Vect n e) -> Vect n (Vect m e)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTransposed = transposeMat xs in
                             transposeHelper x xsTransposed

total
addVec : Num e => Vect n e -> Vect n e -> Vect n e
addVec [] [] = []
addVec (x :: xs) (y :: ys) = x + y :: addVec xs ys

total
addMat : Num e => Vect n (Vect m e) -> Vect n (Vect m e) -> Vect n (Vect m e)
addMat [] [] = []
addMat (x ::xs) (y :: ys) = addVec x y :: addMat xs ys

total
multVec : Num e => Vect n e -> Vect n e -> e
multVec [] [] = 0
multVec (x :: xs) (y :: ys) = x * y + multVec xs ys

total
multMat : Num e => Vect n (Vect m e) -> Vect m (Vect p e) -> Vect n (Vect p e)
multMat [] [] = []
multMat (x :: xs) (y :: ys) = ?mm
multMat xs ys = ?mmm

multMatT : Vect n (Vect m e) -> Vect p (Vect m e) -> Vect p (Vect n e)
multMatT xs [] = []
multMatT [] (y :: ys) = ?sdda
multMatT (x :: xs) (y :: ys) = ?mmt

--transposeVec : Vect n e -> Vect n (Vect 1 e)
--
--transposeMat : Vect n (Vect m e) -> Vect m (Vect n e)
--transposeMat [] = []
--transposeMat (x :: xs) = ?transposeVec
