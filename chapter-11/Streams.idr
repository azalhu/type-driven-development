module Streams

import Data.Stream

total
labelWith : Stream labelType -> List a -> List (labelType, a)
labelWith lbls [] = []
labelWith (lbl :: lbls) (val :: vals) = (lbl, val) :: labelWith lbls vals

total
label : List a -> List (Integer, a)
label = labelWith (iterate (+ 1) 0)

