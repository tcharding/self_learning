import System.Random
import Data.List
{-
Problem 21
Insert an element at a given position into a list.
-}
insertAt :: a -> [a] -> Int -> [a]
insertAt y xs@(x:xs') n
  | n == 1 = y:xs
  | otherwise = x:insertAt y xs' (n-1)

{-
Problem 22
Create a list containing all integers within a given range.
-}
range :: Int -> Int -> [Int]
range s f = helper [] s f
  where helper acc s f
          | s == (f+1) = reverse acc
          | otherwise = helper (s:acc) (s+1) f

{-
Problem 23
Extract a given number of randomly selected elements from a list.
-}
randomSelect :: [a] -> Int -> IO [a]
randomSelect xs n = do
  gen <- getStdGen
  return $ take n [xs !! x | x <- randomRs (0, (length xs) -1) gen]

{-
Problem 24
Lotto: Draw N different random numbers from the set 1..M
-}
lotto :: (Num a, Enum a) => a -> Int -> IO [a]
lotto m n = do
  gen <- getStdGen
  return $ take n [xs !! x | x <- randomRs (0, (length xs) -1) gen]
    where xs = [1..m]

{-
Problem 25
Generate a random permutation of the elements of a list.
-}
randomPermu :: [a] -> IO [a]
randomPermu [] = return []
randomPermu (x:xs) = do
  rand <- randomRIO (0, (length xs))
  rest <- randomPermu xs
  return $ let (ys,zs) = splitAt rand rest
           in ys++(x:zs)

randomPermu' [] = return []
randomPermu' xs = do
    rand <- randomRIO (0, (length xs)-1)
    rest <- let (ys,(_:zs)) = splitAt rand xs
            in randomPermu' $ ys ++ zs
    return $ (xs!!rand):rest

{-
Problem 26
(**) Generate the combinations of K distinct objects chosen from the N elements of a list
-}
--genKDistinctObjects n xs = combinations $ rmNonDistinct $ genKObjects n xs
genKDistinctObjects n xs = combinations $ filter isDistinct $ genKObjects n xs

-- get combinatons from permutations
combinations :: Eq a => [[a]] -> [[a]]
combinations (x:xs)
  | containsSimilar x xs = combinations xs
  | otherwise = x : combinations xs
combinations _ = []

containsSimilar :: Eq a => [a] -> [[a]] -> Bool
containsSimilar y (x:xs)
  | isSimilar y x = True
  | otherwise = containsSimilar y xs
containsSimilar _ [] = False

isSimilar :: Eq a => [a] -> [a] -> Bool
isSimilar a b = containsEveryElement a b && containsEveryElement b a
  where containsEveryElement (x:xs) ys = (elem x ys) && (containsEveryElement xs ys)
        containsEveryElement _ _ = True 

-- genereate all permutations of length k from list
genKObjects :: Int -> [a] -> [[a]]
genKObjects 1 xs = individualElements xs
genKObjects k xs = addEach xs (genKObjects (k-1) xs)

-- list is distinct if all elements appear only once
isDistinct :: Eq a => [a] -> Bool
isDistinct [] = True
isDistinct (x:xs)
  | x `elem` xs = False
  | otherwise = isDistinct xs

individualElements :: [a] -> [[a]]
individualElements [] = []
individualElements (x:xs) = ([x]) : individualElements xs

addEach :: [a] -> [[a]] -> [[a]]
addEach (x:xs) ys = map (x:) ys ++ addEach xs ys 
addEach _ _ = []

{-
Problem 27
Group the elements of a set into disjoint subsets.

a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3
and 4 persons? Write a function that generates all the possibilities and returns
them in a list.
-}

-- did not complete

{-
Problem 28
Sorting a list of lists according to length of sublists

a) We suppose that a list contains elements that are lists themselves. The
objective is to sort the elements of this list according to their
length. E.g. short lists first, longer lists later, or vice versa.
-}
lsort :: [[a]] -> [[a]]
lsort = sortBy cmp
  where cmp a b = length a `compare` length b
