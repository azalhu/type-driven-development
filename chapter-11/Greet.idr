module Greet

import InfIO

total
greet : InfIO
greet = do
  putStr "Enter your name: "
  name <- getLine
  putStrLn ("Hello " ++ name)
  greet

