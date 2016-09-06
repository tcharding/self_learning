import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

          -- replace "id" with name of function to run
        myFunction = firstWord

firstWord :: String -> String
firstWord [] = []
firstWord cs = unlines $ helper $ lines cs
  where helper [] = []
        helper (l:ls) = (head $ words l) : (helper ls)
