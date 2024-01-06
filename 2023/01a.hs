-- contents <- readFile "01a.txt"

main = do
  contents <- readFile "01a.txt"
  let line <- words contents
  putStrLn w
  -- putStrLn "Hello, everybody!"
  -- putStrLn ("Please look at my favorite odd numbers: " ++ show (filter odd [10..20]))