{-# OPTIONS_GHC -Wall #-}
import Data.List (takeWhile)
-- Exercise 1

{-
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs
-}

fun1 :: [Integer] -> Integer
fun1 = foldr f 1 . filter even
  where f x acc = acc * (2 - x)

{-
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)
-}

fun2 :: Integer -> Integer
fun2 n = sum $ takeWhile (/= 1) $ iterate collatz n

collatz :: Integer -> Integer
collatz n
  | even n = div n 2
  | otherwise = 3 * n + 1

-- Exercise 2

-- Integer at each node represents height of node
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- generate a balanced binary tree from a list of values using foldr.
foldTree :: [a] -> Tree a
foldTree = undefined


-- Exercise 3

{- xor [False, True, False] == True
   xor [False, True, False, False, True] == False -}

xor :: [Bool] -> Bool
xor = foldr f False
  where f False acc = acc
        f True acc = not (True && acc)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr g []
  where g a bs = f a : bs

--myFoldl :: (a -> b -> a) -> a -> [b] -> a
--myFoldl f base xs = foldr g

-- Exercise 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [i + j + 2 * i * j
  -- i + j + 2ij
  
  where f x = not $ odd x

{- cartProd [1,2] [’a’,’b’] == [(1,’a’),(1,’b’),(2,’a’),(2,’b’)] -}

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
