main = do
  putStrLn "hello, what is your name?"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", you rule!")
