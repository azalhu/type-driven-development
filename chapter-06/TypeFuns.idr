module TypeFuns

import Data.String

total
StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

total
getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "NinetyFour"
getStringOrInt True = 94

total
valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False str = trim str
valToString True n = cast n

total
valToString' : (isInt : Bool) ->
               (case isInt of
                     False => String
                     True => Int) ->
               String
valToString' False str = trim str
valToString' True n = cast n

total
valToString'' : (isInt : Bool) -> (if isInt then Int else String) -> String
valToString'' False str = trim str
valToString'' True n = cast n
