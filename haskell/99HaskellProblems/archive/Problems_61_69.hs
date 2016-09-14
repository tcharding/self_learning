import Data.List
import BST

-- H-99: Ninety-Nine Haskell Problems
-- https://wiki.haskell.org/99_questions

tree4 = Node 1 (Node 2 Nil (Node 4 Nil Nil))
                 (Node 2 Nil Nil)

-- miscellanious tree functions

-- value held in tree not important to these problems
type T = Tree Char              
leaf = Node 'x' Nil Nil

-- add node to leftmost position
addNodeL :: T -> T
addNodeL Nil = leaf
addNodeL (Node v l r) = (Node v (addNodeL l) r)

-- add node to rightmost position
addNodeR :: T -> T
addNodeR Nil = leaf
addNodeR (Node v l r) = (Node v l (addNodeR r))

-- add node to tree
addNode :: T -> T
addNode Nil = leaf
addNode t@(Node v l r)
  | isFull t = addNodeL t
  | size l < size r = Node v (addNode l) r
  | otherwise = Node v l (addNode r)

construct :: Ord a => [a] -> Tree a
construct xs = addAlt Nil (reverse front) back
  where (front,back) = splitInHalf $ sort xs

addAlt :: Ord a => Tree a -> [a] -> [a] -> Tree a
addAlt t (x:xs) (y:ys) = addAlt (add y (add x t) ) xs ys
addAlt t [] [y] = add y t
addAlt t [x] [] = add x t
addAlt t [] [] = t

splitInHalf :: [a] -> ([a],[a])
splitInHalf xs
  | even (length xs) = ((take n xs),(drop n xs))
  | otherwise = ((take (n+1) xs),(drop (n+1) xs))
    where n = (quot (length xs ) 2)

{-
Problem 61
Count the leaves of a binary tree
-}
nLeaves :: Tree a -> Int
nLeaves Nil = 0
nLeaves (Node _ Nil Nil) = 1
nLeaves (Node _ l r) = nLeaves l + nLeaves r
-- countLeaves = length $ leaves

{-
Problem 61A
Collect the leaves of a binary tree in a list
-}

leaves :: Tree a -> [a]
leaves Nil = []
leaves (Node v Nil Nil) = [v]
leaves (Node v l r) = leaves l ++ leaves r

{-
Problem 62
Collect the internal nodes of a binary tree in a list
-}
internals :: Tree a -> [a]
internals (Node _ Nil Nil) = []
internals Nil = []
internals (Node v l r) = v : internals l ++ internals r

{-
Problem 62B
Collect the nodes at a given level in a list
-}
atLevel :: Tree a -> Int -> [a]
atLevel t n = helper t 1
  where helper (Node v l r) pos
          | pos < n = (helper l (pos+1)) ++ (helper r (pos+1))
          | pos == n = v : (helper l (pos+1)) ++ (helper r (pos+1))
        helper _ _ = []

{-
Problem 63
Construct a complete binary tree We can assign an address number to
each node in a complete binary tree by enumerating the nodes in level-order,
starting at the root with number 1. For every node X with address A the
following property holds: The address of X's left and right successors are 2*A
and 2*A+1, respectively, if they exist. This fact can be used to elegantly
construct a complete binary tree structure.
-}

-- from solutions
cTree :: Int -> Tree Char
cTree n = genTree 1
  where genTree x
          | x > n = Nil
          | otherwise = Node 'x' (genTree (2*x)) (genTree (2*x+1))
  
{-

-}
