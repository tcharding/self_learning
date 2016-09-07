import Data.Char (digitToInt,isDigit)
{-
Reverse Polish Notation Calculator

input: "10 4 3 + 2 * -"
ouput: -4
-}

rpn :: (Num a, Read a) => String -> Double
rpn = head . foldl step [] . words
  where step (x:y:ys) "*" = (x * y):ys
        step (x:y:ys) "+" = (x + y):ys
        step (x:y:ys) "-" = (y - x):ys
        step (x:y:ys) "/" = (x / y):ys
        step xs "sum" = [sum xs]
        step (x:y:ys) "^" = (x ** y):ys
        step (x:xs) "ln" = log x:xs
        step xs numString = read numString:xs
