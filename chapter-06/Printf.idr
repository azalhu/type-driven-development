module Printf

import Data.String

data Format = ||| %d - Integer
            Number Format
            | ||| %s - String
            Str Format
            | ||| %c - Char
            Ch Format
            | ||| %f - Double
            Dbl Format
            | ||| literal string
            Lit String Format
            | ||| end of format specifier
            End

total
PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
PrintfType (Ch fmt) = (c : Char) -> PrintfType fmt
PrintfType (Dbl fmt) = (d : Double) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String

total
printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Ch fmt) acc = \c => printfFmt fmt (acc ++ singleton c)
printfFmt (Dbl fmt) acc = \d => printfFmt fmt (acc ++ show d)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt End acc = acc

total
toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Ch (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Dbl (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                             Lit lit chars' => Lit (strCons c lit) chars'
                             fmt => Lit (singleton c) fmt

total
printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf _ = printfFmt _ ""

total
test : IO ()
test = do
  putStrLn (printf "Hello!")
  putStrLn (printf "Answer: %d" 42)
  putStrLn (printf "%s number %d" "Page" 94)
  putStrLn (printf "%c %f" 'X' 42)
