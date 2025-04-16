module PrintLength

printLength : IO ()
printLength = putStr "Input string: " >>= \_ =>
              getLine >>= \input =>
              let len = length input in
              putStrLn (show len)

printLength' : IO ()
printLength' = putStr "Input string: " >>
               getLine >>= \input =>
               putStrLn (show (length input))

printLength'' : IO ()
printLength'' = do
  putStr "Inpute string: "
  input <- getLine
  let len = length input
  putStrLn (show len)

printTwoThings : IO ()
printTwoThings = do putStrLn "Hello"
                    putStrLn "World"

printInput : IO ()
printInput = do x <- getLine
                putStrLn x

