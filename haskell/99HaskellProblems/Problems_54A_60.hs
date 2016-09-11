import Data.List
-- H-99: Ninety-Nine Haskell Problems
-- https://wiki.haskell.org/99_questions

{-
Problem 55
(**) Construct completely balanced binary trees

In a completely balanced binary tree, the following property holds for every
node: The number of nodes in its left subtree and the number of nodes in its
right subtree are almost equal, which means their difference is not greater than
one.

Write a function cbal-tree to construct completely balanced binary trees for a
given number of nodes. The predicate should generate all solutions via
backtracking. Put the letter 'x' as information into all nodes of the tree.
-}

data Tree a = Branch a (Tree a) (Tree a)
            | Empty
              deriving (Show,Eq)

leaf x = Branch x Empty Empty

cbalTree :: Int -> [Tree Char]
cbalTree 1 = [leaf 'x']
cbalTree n = cull $ addNode (cbalTree (n-1))

cull :: Eq a => [Tree a] -> [Tree a]
cull [] = []
cull (t:ts) = t : (cull $ delete t ts)

addNode :: Eq a => Tree a -> [Tree a]
addNode = 
                                             

-- test data
x = 1
t1 = (Branch x (leaf x) Empty)
t2 = (Branch x Empty (leaf x))
t3 = (Branch x (leaf x) (Branch x (leaf x) Empty))
