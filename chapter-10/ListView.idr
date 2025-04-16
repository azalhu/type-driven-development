module ListView

total
data ListView : List a -> Type where
  (++) : (left : List a) -> (right : List a) -> ListView (left ++ right)

total
left : {x : a} -> ListView (x :: xs) -> List a
left  lv = ?rhs

--total
--right : ListView a -> List a
--right (_ ++ right') = right'
--
--total
--break : ListView Integer -> (List Integer, List Integer)
--break (left ++ right) = (left, right)
--
--total
--mkLeft : List Integer
--mkLeft = [1, 2, 3, 4]
--
--total
--mkRight : List Integer
--mkRight = [5, 6, 7]
--
--total
--listView : ListView Integer
--listView = mkLeft ++ mkRight
