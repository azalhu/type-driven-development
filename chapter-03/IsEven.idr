module IsEven

mutual
  total
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k

  total
  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k
