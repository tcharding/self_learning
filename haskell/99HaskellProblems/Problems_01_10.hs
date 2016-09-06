-- H-99: Ninety-Nine Haskell Problems
-- https://wiki.haskell.org/99_questions
module Problems_01_10 ( group',
                        encode
                      )
  where

{-
Problem 1
(*) Find the last element of a list.
-}
last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (x:xs) = last' xs

{-
Problem 2
(*) Find the last but one element of a list.
-}
lastButOne :: [a] -> a
lastButOne [] = error "empty list"
lastButOne [x] = error "one element only"
lastButOne (x:y:[]) = x
lastButOne (x:xs) = lastButOne xs

{-
Problem 3
(*) Find the K'th element of a list. The first element in the list is number 1.
-}
kthElement :: Int -> [a] -> a
kthElement _ []  = error "list too short"
kthElement 1 (x:_) = x
kthElement k (x:xs) = kthElement (k - 1) xs

{-
Problem 4
(*) Find the number of elements of a list.
-}
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

{-
Problem 5
(*) Reverse a list.
-}
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

{-
Problem 6
(*) Find out whether a list is a palindrome. A palindrome can be read forward or
backward; e.g. (x a m a x).
-}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = reverse xs == xs

{-
Problem 7
(**) Flatten a nested list structure.
-}
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (nl:nls)) = flatten nl ++ flatten (List nls)
flatten (List []) = []

{-
Problem 8
(**) Eliminate consecutive duplicates of list elements.
-}
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:ys)
  | x == y = compress (y:ys)
  | otherwise = x : compress (y:ys)

{-
Problem 9
(**) Pack consecutive duplicates of list elements into sublists. If a list
contains repeated elements they should be placed in separate sublists.
-}
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (x:xs) = (x:ys) : group' zs
  where (ys,zs) = span (==x) xs

{-
Problem 10
(*) Run-length encoding of a list. Use the result of problem P09 to implement
the so-called run-length encoding data compression method. Consecutive
duplicates of elements are encoded as lists (N E) where N is the number of
duplicates of the element E.
-}
encode :: Eq a => [a] -> [(Int,a)]
encode = helper . group'
  where helper [] = []
        helper (x:xs) = ((length x),(head x)) : helper xs
