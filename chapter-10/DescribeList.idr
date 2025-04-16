module DescribeList

public export
total
data ListLast : List a -> Type where
  Empty : ListLast Nil
  NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ (x :: Nil))

export
total
listLast : (xs : List a) -> ListLast xs
listLast Nil = Empty
listLast (x :: Nil) = NonEmpty Nil x
listLast (x :: xs) = case listLast xs of
                          Empty => NonEmpty Nil x
                          NonEmpty ys y => NonEmpty (x :: ys) y

total
describeListEndExpl : List Int -> String
describeListEndExpl input with (listLast input)
  describeListEndExpl [] | Empty = "Empty"
  describeListEndExpl (xs ++ [x]) | NonEmpty xs x = "Non-empty, initial portion: " ++ show xs

total
describeListEndImpl : List Int -> String
describeListEndImpl input with (listLast input)
  describeListEndImpl _ | Empty = "Empty"
  describeListEndImpl _ | NonEmpty xs x = "Non-empty, initial portion: " ++ show xs

total
describeListEnd : List Int -> String
describeListEnd = describeListEndExpl

covering
myReverse : List a -> List a
myReverse input with (listLast input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | NonEmpty xs x = x :: myReverse xs

