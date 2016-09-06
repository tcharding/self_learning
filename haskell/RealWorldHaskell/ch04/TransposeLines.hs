import System.Environment (getArgs)
import Data.List

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
        myFunction = trans

trans :: String -> String
trans [] = []
trans cs = unlines $ transpose $ lines cs
