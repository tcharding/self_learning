import Data.Char (digitToInt,isSpace)
--
-- Exercises - Page 84

{-
-- exercise 2
splitWith :: (a -> Bool) -> [a] -> [a]
splitWith pred xs = helper [] pred xs
  where helper acc _ [] = acc
        helper acc pred (x:xs)
          | pred x = helper x:acc pred xs
          | otherwise = helper acc pred xs

--
-- Exercises - Page 84

-- exercise 1 and 2
asInt :: String -> Int
asInt xs = loop 0 xs
  where loop acc [] = acc
        loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                          in loop acc' xs

asIntFold :: String -> Int
asIntFold [] = 0
asIntFold xs@(x:xs')
  | x == '-' = (-1) * helper xs'
  | otherwise = helper xs
  where helper = foldl step 0 
          where step acc x = acc * 10 + digitToInt x
-}
-- exercise 3

asIntFold :: String -> Int
asIntFold [] = 0
asIntFold xs@(x:xs')
  | x == '-' = (-1) * helper xs'
  | otherwise = helper xs
  where helper = foldl step 0 
          where step acc x = acc * 10 + digitToInt x

-- exercise 5
concat' :: [[a]] -> [a]
concat' = foldr step []
  where step acc x = acc ++ x

-- exercise 7
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _  [] =  []
groupBy' eq (x:xs) = (x:ys) : groupBy' eq zs
  where (ys,zs) = span (eq x) xs

--any' :: Foldable t => (a -> Bool) -> t a -> Bool
any' p = foldr step False
  where step x acc
          | p x = True
          | otherwise = acc

-- cannot write cycle with fold because step returns b not [b]
cycle' :: [a] -> [a]
cycle' [] = error "empty list"
cycle' xs = xs'
  where xs' = xs ++ xs'

{-
words' :: String -> [String]
words' "" = []
words' (x:xs)
  | isSpace x =
    -}
sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = [] -- Do nothing to the empty list
sumEveryTwo (x:[])     = [] -- Do nothing to lists with a single element
sumEveryTwo (x:y:zs) = (x + y) : sumEveryTwo zs

--main = print (sumEveryTwo [1,2,3,4,5,6,7,8])
