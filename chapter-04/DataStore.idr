module Main

import Data.String
import Data.Vect
import System.REPL

total
data DataStore : Type where
  MkData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore

total
size : DataStore -> Nat
size (MkData size' _) = size'

total
items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'

total
addToStore : DataStore -> String -> DataStore
addToStore (MkData _ items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (item :: items) = item :: addToData items

total
data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

total
parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "search" str = Just (Search str)
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

total
parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

total
getEntry : (pos : Integer) -> DataStore -> Maybe (String, DataStore)
getEntry pos store = case integerToFin pos (size store) of
                          Nothing => Just ("Index out of range", store)
                          Just idx => Just (index idx (items store), store)

total
searchString : (idx : Nat) -> (items : Vect n String)  -> (str : String) -> String
searchString _ [] _ = ""
searchString idx (item :: items) str = let rest = searchString idx items str in
                                           if isInfixOf str item
                                              then show idx ++ ": " ++ item ++ "\n" ++ rest
                                              else rest

total
searchStore : String -> DataStore -> Maybe (String, DataStore)
searchStore "" store = Just ("You must enter a search term!", store)
searchStore str store = Just (let result = mkResult (getEntries str (items store) 0) in
                                  if result == ""
                                     then "No entries found from the search term"
                                     else result, store)
  where
    getEntries : String -> Vect _ String -> (pos : Nat) -> List (Nat, String)
    getEntries _ [] _ = []
    getEntries str (item :: items) pos = case isInfixOf str item of
                                              False => getEntries str items (S pos)
                                              True => (pos, item) :: getEntries str items (S pos)

    mkResult : List (Nat, String) -> String
    mkResult [] = ""
    mkResult ((pos, item) :: items) = show pos ++ ": " ++ item ++ let result = mkResult items in
                                                                      if result == ""
                                                                         then ""
                                                                         else "\n" ++ result

total
processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                                Nothing => Just ("Invalid command", store)
                                Just (Add str) => Just ("ID " ++ show (size store), addToStore store str)
                                Just (Get pos) => getEntry pos store
                                --Just (Search str) => searchStore str store
                                Just (Search str) => Just (searchString 0 (items store) str, store)
                                Just Size => Just (show (size store), store)
                                Just Quit => Nothing

total
process : DataStore -> String -> Maybe (String, DataStore)
process store input = case processInput store input of
                           Nothing => Nothing
                           Just (output, newStore) => Just (output ++ "\n", newStore)

main : IO ()
main = replWith (MkData _ []) "Command: " process
