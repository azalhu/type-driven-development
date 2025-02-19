module Main

main : IO ()
main = putStrLn (cast 'x')

StringOrInt : Bool -> Type
StringOrInt x = case x of
                     True => Int
                     False => String

getStringOrInt : (x : Bool) -> StringOrInt x
getStringOrInt x = case x of
                        True => 94
                        False => "Ninety four"

getString : String
getString = "Fifty five"

getStringOrIntAlt : (x : Bool) -> StringOrInt x
getStringOrIntAlt True = 55
getStringOrIntAlt False = getString

valToString : (x : Bool) -> StringOrInt x -> String
valToString x val = case x of
                         True => cast val
                         False => val
