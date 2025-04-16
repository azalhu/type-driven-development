module Streams

%default total

labelWith : Stream labelType -> List a -> List (labelType, a)
labelWith lbls [] = []
labelWith (lbl :: lbls) (val :: vals) = (lbl, val) :: labelWith lbls vals

