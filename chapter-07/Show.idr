module Show

import Data.Vect
import Ord

Show Album where
  show (MkAlbum artist title year) = title ++ " by " ++ artist ++ " (released " ++ show year ++ ")"

Num e => Num (Vect 1 e) where
  (+) [x] [y] = [x + y]
  (*) [x] [y] = [x * y]
  fromInteger x = [fromInteger x]

