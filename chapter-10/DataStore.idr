module DataStore

import Data.Vect

infixr 5 .+.

total public export
data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

total public export
SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

total export
record DataStore (schema : Schema) where
  constructor MkData
  size : Nat
  items : Vect size (SchemaType schema)

total export
empty : DataStore schema
empty = MkData 0 []

total export
addToStore : (value : SchemaType schema) -> (store : DataStore schema) -> DataStore schema
addToStore value (MkData _ items) = MkData _ (value :: items)

total public export
data StoreView : {schema : _} -> DataStore schema -> Type where
  SNil : StoreView DataStore.empty
  SAdd : {value, store : _} -> (rec : StoreView store) -> StoreView (addToStore value store)

total private
storeViewHelp : {size : _} ->
                (items' : Vect size (SchemaType schema)) ->
                StoreView {schema} (MkData size items')
storeViewHelp [] = SNil
storeViewHelp (value :: xs) = SAdd (storeViewHelp xs)

total export
storeView : (store : DataStore schema) -> StoreView store
storeView (MkData _ items) = storeViewHelp items

