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
genKDistinctObjects n xs = combinations $ filter isDistinct $ permN n xs
c n xs = genKDistinctObjects n xs

-- get combinatons from permutations
combinations :: Eq a => [[a]] -> [[a]]
combinations (x:xs)
  | xs `holdsPermOf` x = combinations xs
  | otherwise = x : combinations xs
--  | containsSimilar x xs = combinations xs
--  | otherwise = x : combinations xs
combinations _ = []

holdsPermOf :: Eq a => [[a]] -> [a] -> Bool
(x:xs) `holdsPermOf` y
  | isPerm x y = True
  | otherwise = xs `holdsPermOf` y
  where isPerm a b = (a `hasAllElementsOf` b) && (b `hasAllElementsOf` a)
          where xs `hasAllElementsOf` (y:ys) = (y `elem` xs) && (xs `hasAllElementsOf` ys)
                _ `hasAllElementsOf` [] = True 
[] `holdsPermOf` _ = False

-- genereate all permutations of length n
permN :: Int -> [a] -> [[a]]
permN 1 xs = individual xs
  where individual (e:es) = ([e]) : individual es
        individual _ = []
permN n xs = addEach xs (permN (n-1) xs)

-- list is distinct if all elements appear only once
isDistinct :: Eq a => [a] -> Bool
isDistinct [] = True
isDistinct (x:xs)
  | x `elem` xs = False
  | otherwise = isDistinct xs

-- concat individially each element of list to each sublist
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
type Group = String
-- group of people represented as "abcdefghi"
people = "abcdefghi"
-- results of form ("ab","cde","fghi")
solve_27 = triplets $ twins $ genKDistinctObjects 2 people

triplets :: [(Group,Group)] -> [(Group,Group,Group)]
triplets ((a,b):rest) = (a,b,(remaining a b)) : triplets rest
  where remaining a b = ((people `less` a) `less` b)
triplets [] = []

-- form pairs consisting of one group of 2 and one group of 3
twins :: [Group] -> [(Group,Group)]
twins [] = []
twins (x:xs) = formTwins x ++ twins xs
  where formTwins g = combine g $ genKDistinctObjects 3 (people `less` g)
          where combine g (t:ts) = (g,t):combine g ts
                combine _ [] = []

-- remove all characters of h from g
less :: String -> String -> String
g `less` "" = g
g `less` (h:hs) = (delete h g) `less` hs

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
