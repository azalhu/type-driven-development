module TestStore

import DataStore

total private
testStore : DataStore (SString .+. SString .+. SInt)
testStore = addToStore ("Mercury", "Mariner 10", 1974) $
            addToStore ("Venus", "Venera", 1961) $
            addToStore ("Uranus", "Voyager 2", 1986) $
            addToStore ("Pluto", "New Horizons", 2015) $
            empty

total private
listItems : DataStore schema -> List (SchemaType schema)
listItems input with (storeView input)
  listItems DataStore.empty | SNil = []
  listItems (addToStore value store) | SAdd rec = value :: listItems store | rec

total private
filterKeys : (test : SchemaType val_schema -> Bool) ->
             DataStore (SString .+. val_schema) ->
             List String
filterKeys test store with (storeView store)
  filterKeys test DataStore.empty | SNil = []
  filterKeys test (addToStore (key, value) store) | SAdd rec =
    case test value of
         False => filterKeys test store | rec
         True => key :: filterKeys test store | rec

