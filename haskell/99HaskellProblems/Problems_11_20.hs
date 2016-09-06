-- H-99: Ninety-Nine Haskell Problems
-- https://wiki.haskell.org/99_questions
import Problems_01_10

{-
Problem 11
(*) Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has no
duplicates it is simply copied into the result list. Only elements with
duplicates are transferred as (N E) lists.
-}

--data NestedList a = Elem a | List [NestedList a]
data ListItem a = Single a | Multiple Int a
  deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map helper . encode
  where 
    helper (1,x) = Single x
    helper (n,x) = Multiple n x

{-
Problem 12
(**) Decode a run-length encoded list.

Given a run-length code list generated as specified in problem 11. Construct its
uncompressed version.
-}
decode :: Eq a => [ListItem a] -> [a]
decode = concat . map helper
  where helper (Single x) = [x]
        helper (Multiple n x) = replicate n x

{-
Problem 13
(**) Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression method
directly. I.e. don't explicitly create the sublists containing the duplicates,
as in problem 9, but only count them. As in problem P11, simplify the result
list by replacing the singleton lists (1 X) by X.
-}

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs) = pushElemOnFront x (encodeDirect xs)
  where pushElemOnFront e [] = [Single e]
        pushElemOnFront e (li:lis)
          | isSameItem e li = (addElemToItem e li) : lis
          | otherwise = Single e : li : lis

buildListItem :: Eq a => (Int,a) -> ListItem a
buildListItem (n,x)
  | n == 1 = Single x
  | otherwise = Multiple n x

isSameItem :: Eq a => a -> ListItem a -> Bool
isSameItem e (Single x) = e == x
isSameItem e (Multiple n x) = e == x

-- element must have same value as ListItem element
addElemToItem :: Eq a => a -> ListItem a -> ListItem a
addElemToItem e (Single x) = Multiple 2 x
addElemToItem e (Multiple n x) = Multiple (n+1) x
  
{-
Problem 14
(*) Duplicate the elements of a list.
-}
dupElems :: [a] -> [a]
dupElems [] = []
dupElems (x:xs) = x : x : dupElems xs

{-
Problem 15
(**) Replicate the elements of a list a given number of times.
-}
replicateList :: [a] -> Int -> [a]
replicateList xs n = concat $ helper xs n
  where helper (x:xs) n = (replicate n x) : helper xs n
        helper _ _ = []

{-
Problem 16
(**) Drop every N'th element from a lis
-}
dropNth :: [a] -> Int -> [a]
dropNth xs n = helper 1 n xs
  where helper i n (x:xs)
          | i == n = helper 1 n xs
          | otherwise = x : helper (i+1) n xs
        helper _ _ _ = []

{-
Problem 17
(*) Split a list into two parts; the length of the first part is given.

Do not use any predefined predicates.
-}
splitByN :: [a] -> Int -> ([a],[a])
splitByN xs n = helper [] n xs
  where helper first n all@(x:xs)
          | n == 0 = (first,all)
          | otherwise = helper (first ++ [x]) (n-1) xs

{-
Problem 18
(**) Extract a slice from a list.

Given two indices, i and k, the slice is the list containing the elements
between the i'th and k'th element of the original list (both limits
included). Start counting the elements with 1.

-}
slice :: [a] -> Int -> Int -> [a]
slice (x:xs) i j
  | j == 2 = []
  | i == 1 = x : slice xs i (j-1)
  | otherwise = slice xs (i-1) j

{-
Problem 19
(**) Rotate a list N places to the left.
-}
rotate :: [a] -> Int -> [a]
rotate xs n
  | neg n = rotateR xs (abs n)
  | otherwise = rotateL xs n

neg :: Int -> Bool
neg n = n < 0

rotateR :: [a] -> Int -> [a]
rotateR xs n = drop r xs ++ take r xs
  where r = (length xs) - n

rotateL :: [a] -> Int -> [a]
rotateL xs n = drop n xs ++ take n xs

{-
Problem 20
(*) Remove the K'th element from a list.
-}
removeAt :: Int -> [a] -> (a,[a])
removeAt k xs = ((kthElement k xs),(removeKthElement k xs))

kthElement :: Int -> [a] -> a
kthElement k (x:xs)
  | k == 1 = x
  | otherwise = kthElement (k-1) xs
kthElement _ _ = error "not enough elements in list"  

removeKthElement :: Int -> [a] -> [a]
removeKthElement k (x:xs)
  | k == 1 = xs
  | otherwise = x : removeKthElement (k-1) xs
removeKthElement _ _ = error "not enough elements in list"  
