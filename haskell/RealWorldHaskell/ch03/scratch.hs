import Data.List

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

mean xs = (sum xs) / fromIntegral (length xs)

makePalandrome xs = xs ++ reverse xs

isPalindrome xs = xs == reverse xs

sortByLength xs = sortBy cmp xs
  where cmp a b = length a `compare` length b

intersperse' :: a -> [[a]] -> [a]
intersperse' _ [] = []
intersperse' _ [string] = string
--intersperse' sep (s:ss) = s:sep:intersperse' sep ss

-- return list with smallest element at front
smallest :: (Ord a) => [a] -> [a]
smallest [x] = [x]
smallest (x:xs)
  | x < head parsed = x:parsed
  | otherwise = (head parsed):x:(tail parsed)
  where parsed = smallest xs
