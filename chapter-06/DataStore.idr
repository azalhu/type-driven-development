module DataStore

import Data.String
import Data.Vect
import System.REPL

infixr 5 .+.

total
data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

total
SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

total
record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

total
GetEntries : Maybe Integer -> DataStore -> Type
GetEntries Nothing store = Vect (size store) (SchemaType (schema store))
GetEntries (Just pos) store = Either String (SchemaType (schema store))

total
setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              S _ => Nothing

total
addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema' size store) newItem = MkData _ _ (addToData store)
  where
    addToData : Vect oldSize (SchemaType schema') -> Vect (S oldSize) (SchemaType schema')
    addToData [] = [newItem]
    addToData (item :: items) = item :: addToData items

total
getEntries : (pos : Maybe Integer) -> (store : DataStore) -> GetEntries pos store
getEntries Nothing store = items store
getEntries (Just pos) store = let store_items = items store in
                                  case integerToFin pos (size store) of
                                       Nothing => Left "Index out of range"
                                       Just id => Right (index id store_items)

total
display : {schema : _} -> SchemaType schema -> String
display {schema = SString} item = item
display {schema = SInt} item = show item
display {schema = SChar} item = "'" ++ singleton item ++ "'"
display {schema = (_ .+. _)} (iteml, itemr) = display iteml ++ ", " ++ display itemr

total
searchString : (store : DataStore) -> (str : String) -> (idx : Nat) -> String
searchString (MkData _ _ items) str idx = searchStore items str idx
  where
    searchStore : {schema' : _} -> Vect size' (SchemaType schema') -> (str : String) -> (idx : Nat) -> String
    searchStore [] _ _ = ""
    searchStore (item :: items) str idx = let rest = searchStore items str (S idx)
                                              item' = display item in
                                              if isInfixOf str item'
                                                 then show idx ++ ": " ++ item' ++ "\n" ++ rest
                                                 else rest

total
data Command : Schema -> Type where
  SetSchema : (newSchema : Schema) -> Command schema'
  Add : SchemaType schema' -> Command schema'
  Get : Maybe Integer -> Command schema'
  Search : String -> Command schema'
  Size : Command schema'
  Quit : Command schema'

total
parseSchema : List String -> Maybe Schema
parseSchema (x :: xs) = do
  schema <- case x of
                 "String" => Just SString
                 "Int" => Just SInt
                 "Char" => Just SChar
                 _ => Nothing
  case xs of
       [] => Just schema
       _ => do
         rest <- parseSchema xs
         Just (schema .+. rest)
parseSchema _ = Nothing

total
parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case break (== '"') xs of
                                 (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                 _ => Nothing
    getQuoted _ = Nothing
parsePrefix SInt input = case span isDigit input of
                              ("", rest) => Nothing
                              (num, rest) => Just (cast num, ltrim rest)
parsePrefix SChar input = parseChar (unpack input)
  where
    parseChar : List Char -> Maybe (Char, String)
    parseChar (x :: ' ' :: rest) = Just (x, ltrim (pack rest))
    parseChar (x :: y :: rest) = Nothing
    parseChar (x :: empty@[]) = Just (x, pack empty)
    parseChar _ = Nothing
parsePrefix (schemal .+. schemar) input = do
  (l_val, input') <- parsePrefix schemal input
  (r_val, rest) <- parsePrefix schemar input'
  Just ((l_val, r_val), ltrim rest)

total
parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  Just _ => Nothing
                                  Nothing => Nothing

total
parseDigits : String -> Maybe Integer
parseDigits val = case all isDigit (unpack val) of
                       False => Nothing
                       True => Just (cast val)

total
parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "schema" rest = do
  restok <- parseSchema (words rest)
  Just (SetSchema restok)
parseCommand schema "add" rest = do
  restok <- parseBySchema schema rest
  Just (Add restok)
parseCommand schema "get" "" = Just (Get Nothing)
parseCommand schema "get" val = do
  valok <- parseDigits val
  Just (Get (Just valok))
parseCommand schema "search" str = Just (Search str)
parseCommand schema "size" "" = Just Size
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

total
parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)

total
processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse (schema store) input of
                                Nothing => Just handleNothing
                                Just (SetSchema schema) => Just (handleSetSchema schema)
                                Just (Add item) => Just (handleAdd item)
                                Just (Get pos) => Just (handleGet pos)
                                Just (Search str) => Just (handleSearch str)
                                Just Size => Just handleSize
                                Just Quit => Nothing
  where
    handleNothing : (String, DataStore)
    handleNothing = ("Invalid command", store)

    handleSetSchema : Schema -> (String, DataStore)
    handleSetSchema schema = case setSchema store schema of
                                  Nothing => ("Can't update schema", store)
                                  Just store' => ("OK", store')

    handleAdd : SchemaType (schema store) -> (String, DataStore)
    handleAdd item = ("ID " ++ show (size store), addToStore store item)

    handleGet : Maybe Integer -> (String, DataStore)
    handleGet pos = let items = getEntries pos store in
                        case pos of
                             Nothing => (unlines (toList (map display items)), store)
                             Just _ => case items of
                                            Left err => (err, store)
                                            Right item => (display item, store)

    handleSearch : String -> (String, DataStore)
    handleSearch str = (searchString store str 0, store)

    handleSize : (String, DataStore)
    handleSize = (show (size store), store)

total
process : DataStore -> String -> Maybe (String, DataStore)
process store input = case processInput store input of
                           Nothing => Nothing
                           Just (output, newStore) => Just (output ++ "\n", newStore)

main : IO ()
main = replWith (MkData SString _ []) "Command: " process

test : IO ()
test = do
  let store = MkData SString _ []

  let Just (output, store) = process store "schema String String Char Int Char"
  | Nothing => pure ()
  putStrLn ("schema:\n" ++ output)

  let Just (output, store) = process store "add \"Rain Dogs\" \"Tom Waits\" x 1985 u"
  | Nothing => pure ()
  putStrLn ("add:\n" ++ output)

  let Just (output, store) = process store "add \"Fog on the Tyne\" \"Lindisfarne\" \h  1971 \g "
  | Nothing => pure ()
  putStrLn ("add:\n" ++ output)

  let Just (output, store) = process store "get 0"
  | Nothing => pure ()
  putStrLn ("get 0:\n" ++ output)

  let Just (output, store) = process store "get 1"
  | Nothing => pure ()
  putStrLn ("get 1:\n" ++ output)

  let Just (output, store) = process store "get"
  | Nothing => pure ()
  putStrLn ("get:\n" ++ output)

  let Just (output, store) = process store "search o"
  | Nothing => pure()
  putStrLn ("search o:\n" ++ output)

  let Just (output, store) = process store "schema String String Int"
  | Nothing => pure ()
  putStrLn ("schema:\n" ++ output)

