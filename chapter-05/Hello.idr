module Main

--total
--putHelloName : String -> IO ()
--putHelloName x = putStrLn ("Hello " ++ x ++ "!!!")

--total
--main : IO ()
--main = putStr "Enter your name: " >> getLine >>= putHelloName

total
main : IO ()
main = do
  putStr "Enter your name: "
  x <- getLine
  putStrLn ("Hello " ++ x ++ "!")
