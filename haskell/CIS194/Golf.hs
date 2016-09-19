-- Week 3 - Code Golf

module Golf where

import Data.Map (fromListWith, toList)
import Data.List (intersperse)

-- Exercise 1

{- skips "ABCD" == ["ABCD", "BD", "C", "D"]
   skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
   skips [1] == [[1]]
   skips [True,False] == [[True,False], [False]]
   skips [] == [] -}

-- bad variable names, this is code golf
-- l = xs
-- d = length

-- build list by calling 'f' on the input list in a loop from length of input down to 1
-- pass n as extra argument to save n for call to restart of recursion.
skips :: [a] -> [[a]]
skips xs = reverse $ [f xs n n | n <- [d,(d-1)..1]]
-- skips xs = reverse $ [f xs n | n <- [d,(d-1)..1]]
  where d = length xs

-- keep every nth element of input list
f :: [a] -> Int -> Int -> [a]
f (x:xs) n 1 = x : f xs n n
f (x:xs) n c = f xs n (c-1)
f _ _ _ = []        

{-
f :: [a] -> Int -> [a]
f xs n = g xs n
  where g (x:xs) 1 = x : g xs n
        g (x:xs) c = g xs (c-1)
        g [] _ = []
-}

-- Exercise 2

{- localMaxima [2,9,5,6,1] == [9,6]
   localMaxima [2,3,4,1,5] == [4]
   localMaxima [1,2,3,4,5] == [] -}

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | y > x && y > z = y : localMaxima (z:zs)
  | otherwise = localMaxima (y:z:zs)
localMaxima _ = []  

-- Exercise 3

histogram :: [Int] -> String
histogram xs = concat (intersperse
                       "\n"
                       (map (++ "\n") [stars n ps | n <- [m,(m-1)..1]]))
               ++ "==========\n0123456789\n"
  where ps = frequency xs
        m = maxF ps

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

-- find frequency of most frequent, defaults to 0 if none
maxF :: [(Int,Int)] -> Int
maxF = go 0
  where go mf ((x,f):ps)
          | f > mf = go f ps
          | otherwise = go mf ps
        go mf [] = mf

-- return string with a star in each position if frequency is <= n
stars :: Int -> [(Int,Int)] -> String
stars n freq = go $ fillZeros freq
  where go ((x,f):ps)
          | n <= f = "*" ++ go ps
          | otherwise = " " ++ go ps
        go _ = []

fillZeros :: [(Int,Int)] -> [(Int,Int)]
fillZeros = go 0
  where go c ps@(p@(x,_):ps')
          | x > c = (0,0) : go (c+1) ps
          | otherwise = p : go (c+1) ps'
        go 9 _ = [(9,0)]        -- base case, only gets here when '9' is not present
        go c [] = (c,0) : go (c+1) []

