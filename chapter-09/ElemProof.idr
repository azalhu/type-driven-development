module ElemProof

import Data.Vect
import Data.Vect.Elem
import Decidable.Equality
import RemoveElem

total
one : Integer
one = 1

total
two : Integer
two = 2

total
three : Integer
three = 3

total
four : Integer
four = 4

total
numbers : Vect 3 Integer
numbers = [1, 2, 3]

total
isOneInNumbers : Dec (Elem 1 [1, 2, 3])
isOneInNumbers = isElem one numbers

total
isTwoInNumbers : Dec (Elem 2 [1, 2, 3])
isTwoInNumbers = isElem two numbers

total
isThreeInNumbers : Dec (Elem 3 [1, 2, 3])
isThreeInNumbers = isElem three numbers

total
isFourInNumbers : Dec (Elem 4 [1, 2, 3])
isFourInNumbers = isElem four numbers

total
removeOne : Maybe (Vect 2 Integer)
removeOne = case isOneInNumbers of
                 Yes prf => Just (removeElem' one numbers prf)
                 No contra => Nothing

total
removeTwo : Maybe (Vect 2 Integer)
removeTwo = case isTwoInNumbers of
                 Yes prf => Just (removeElem' two numbers prf)
                 No contra => Nothing

total
removeThree : Maybe (Vect 2 Integer)
removeThree = case isThreeInNumbers of
                   Yes prf => Just (removeElem' three numbers prf)
                   No contra => Nothing

total
removeFour : Maybe (Vect 2 Integer)
removeFour = case isFourInNumbers of
                  Yes prf => Just (removeElem' four numbers prf)
                  No contra => Nothing

total
removeOne' : Vect 2 Integer
removeOne' = removeElem one numbers

total
removeTwo' : Vect 2 Integer
removeTwo' = removeElem two numbers

total
removeThree' : Vect 2 Integer
removeThree' = removeElem three numbers

total
removeFour' : Vect 2 Integer
removeFour' = removeElem four numbers

