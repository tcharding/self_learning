import Data.List
import BST

-- H-99: Ninety-Nine Haskell Problems
-- https://wiki.haskell.org/99_questions

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

-- generate a height complete binary tree with n nodes 
hcTree :: Int -> T
hcTree 0 = Nil
hcTree n = addNode (hcTree (n-1))

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

-- didn't manage to complete this one after two days on it got one buggy solution

-- from solutions
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Nil]
cbalTree n = let (q, r) = (n - 1) `quotRem` 2
             in [Node 'x' left right | i <- [q .. q + r],
                 left <- cbalTree i,
                 right <- cbalTree (n - i - 1)]

{-
Problem 56
(**) Symmetric binary trees

Let us call a binary tree symmetric if you can draw a vertical line through the
root node and then the right subtree is the mirror image of the left
subtree. Write a predicate symmetric/1 to check whether a given binary tree is
symmetric. Hint: Write a predicate mirror/2 first to check whether one tree is
the mirror image of another. We are only interested in the structure, not in the
contents of the nodes.
-}

symmetric :: Eq a => Tree a -> Bool
symmetric Nil = True
symmetric (Node _ l r) = mirror l r

mirror Nil Nil = True
mirror (Node _ a b) (Node _ x y) = mirror a y && mirror b x
mirror _ _ = False

{-
Problem 57
(**) Binary search trees (dictionaries)

Use the predicate add/3, developed in chapter 4 of the course, to write a
predicate to construct a binary search tree from a list of integer numbers.
-}

-- from solutions, is not balanced!
--construct xs = foldl (flip add) Nil xs

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
Problem 58
(**) Generate-and-test paradigm

Apply the generate-and-test paradigm to construct all symmetric, completely
balanced binary trees with a given number of nodes.
-}

symCbalTree :: Int -> [T]
symCbalTree n = filter symmetric $ cbalTree n

{-
Problem 59
(**) Construct height-balanced binary trees

In a height-balanced binary tree, the following property holds for every node:
The height of its left subtree and the height of its right subtree are almost
equal, which means their difference is not greater than one.

Construct a list of all height-balanced binary trees with the given element and
the given maximum height.
-}

-- didn't get this one either

{-
Problem 60
(**) Construct height-balanced binary trees with a given number of nodes

Consider a height-balanced binary tree of height H. What is the maximum number
of nodes it can contain?

Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This
question is more difficult. Try to find a recursive statement and turn it into a
function minNodes that returns the minimum number of nodes in a height-balanced
binary tree of height H. On the other hand, we might ask: what is the maximum
height H a height-balanced binary tree with N nodes can have? Write a function
maxHeight that computes this.  Now, we can attack the main problem: construct
all the height-balanced binary trees with a given number of nodes. Find out how
many height-balanced trees exist for N = 15.
-}

-- oh I think I see a pattern
