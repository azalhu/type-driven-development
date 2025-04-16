module DescribeListArchive

import DescribeList

total
describeHelper' : (input : List Int) -> (form : ListLast input) -> String
describeHelper' Nil Empty = "Empty"
describeHelper' (xs ++ (x :: Nil)) (NonEmpty xs x) = "Non-empty, initial portion: " ++ show xs

total
describeListEnd' : List Int -> String
describeListEnd' xs = describeHelper' xs (listLast xs)

total
describeHelper'' : (input : List Int) -> {auto form : ListLast input} -> String
describeHelper'' Nil {form = Empty} = "Empty"
describeHelper'' (xs ++ (x :: Nil)) {form = NonEmpty xs x} = "Non-empty, initial portion: " ++ show xs

total
describeListEnd'' : List Int -> String
describeListEnd'' xs = let _ = listLast xs in describeHelper'' xs

total
describeListEnd''' : List Int -> String
describeListEnd''' input = case listLast input of
                                Empty => "Empty"
                                NonEmpty xs x => "Non-empty, initial portion: " ++ show xs

