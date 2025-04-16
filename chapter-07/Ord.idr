module Ord

public export
total
record Album where
  constructor MkAlbum
  artist : String
  title : String
  year : Integer

total
help : Album
help = MkAlbum "The Beatles" "Help" 1965

total
rubbersoul : Album
rubbersoul = MkAlbum "The Beatles" "Rubber Soul" 1965

total
clouds : Album
clouds = MkAlbum "Joni Mitchell" "Clouds" 1969

export
total
hunkydory : Album
hunkydory = MkAlbum "David Bowie" "Hunky Dory" 1971

total
heroes : Album
heroes = MkAlbum "David Bowie" "Heroes" 1977

total
collection : List Album
collection = [help, rubbersoul, clouds, hunkydory, heroes]

total
Eq Album where
  (==) a b = artist a == artist b && title a == title b && year a == year b

total
Ord Album where
  compare a b = case compare (artist a) (artist b) of
                     EQ => case compare (year a) (year b) of
                                EQ => compare (title a) (title b)
                                diff_year => diff_year
                     diff_artist => diff_artist

