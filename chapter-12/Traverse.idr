module Traverse

%default total

crew : List String
crew = ["Lister", "Rimmer", "Kryten", "Cat"]

main : IO ()
main = do
  putStr "Display Crew? "
  x <- getLine
  when (x == "yes") (traverse_ putStrLn crew)
  putStrLn "Done"

