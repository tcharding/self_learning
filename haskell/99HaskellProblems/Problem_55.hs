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

-- value of Node is not important to this problem

data Tree a = Node a (Tree a) (Tree a)
            | Nil
              deriving (Eq)

instance Show a => Show (Tree a)
  where show Nil = "Nil"
        show (Node _ Nil Nil) = "Leaf"
        show (Node _ l r) = "(Node " ++ show l ++ " " ++ show r ++ ")"

type T = Tree Char              

--
-- helper functions
--

leaf = Node 'x' Nil Nil

isLeaf :: T -> Bool
isLeaf (Node _ Nil Nil) = True
isLeaf _ = False

isFull :: T -> Bool
isFull Nil = True
isFull (Node _ l r) = isFull l && isFull r && (height l == height r)

isBalanced :: T -> Bool
isBalanced Nil = True
isBalanced (Node _ l r) = isBalanced l && isBalanced r && (height l == height r)

-- height of tree  
height :: T -> Int
height Nil = 0
height (Node _ l r) = 1 + max (height l) (height r)

--
-- Adding Nodes
--

-- add node to leftmost position
addNodeL :: T -> T
addNodeL Nil = leaf
addNodeL (Node v l r) = (Node v (addNodeL l) r)

-- add node to rightmost position
addNodeR :: T -> T
addNodeR Nil = leaf
addNodeR (Node v l r) = (Node v l (addNodeR r))

-- add node to the right of last node
addNodeRH :: T -> T
addNodeRH t
  | isBalanced t = addNodeL t
  | isRightFull t = Nil         -- cannot add node to this tree
  | otherwise = helper t
    where helper Nil = leaf
          helper (Node v l r)
            | isBalanced l && isBalanced r = Node v l (addNodeL r) -- l(h+1), r(h)
            | hasSingleLeftChild l = Node v (helper l) r
            | isRightFull l && isBalanced r = Node v l (addNodeL r)
            | otherwise = Node v l (helper r)

-- Tree contains a Node with a left child and no right child
hasSingleLeftChild :: T -> Bool
hasSingleLeftChild Nil = False
hasSingleLeftChild (Node _ Nil Nil) = False
hasSingleLeftChild (Node _ l Nil) = True
hasSingleLeftChild (Node _ l r) = hasSingleLeftChild l || hasSingleLeftChild r

-- true if right most position of tree contains a leaf
isRightFull :: T -> Bool
isRightFull t
  | isBalanced t = True
isRightFull (Node _ Nil Nil) = True -- base case at far right of tree
isRightFull (Node _ l r) = (height l < height r) && isRightFull r

--
-- Solve the actual problem
--

solve :: Int -> [T]
solve 1 = [leaf]
solve n = nub . concat . map cultivate $ solve (n-1) 

-- build list of all balanced trees by adding node to tree
cultivate :: T -> [T]
cultivate t
  | isBalanced t = helper (addNodeL t)
  | isRightFull t = []
  | otherwise = helper (addNodeRH t)
  where helper Nil = []
        helper t = t : helper (mvR t)

-- move rightmost node right one position
mvR :: T -> T
mvR (Node a x@(Node b Nil Nil) Nil) = Node a Nil x -- left child to right
mvR (Node a (Node b Nil x@(Node _ Nil Nil)) (Node c Nil Nil))
  = Node a (Node b Nil Nil) (Node c x Nil)
mvR (Node a (Node b y@(Node _ Nil Nil) x@(Node _ Nil Nil)) (Node c Nil Nil))
  = Node a (Node b y Nil) (Node c x Nil)
mvR (Node v l r)
  | canMove l = Node v (mvR l) r
  | canMove r = Node v l (mvR r)
  | otherwise = Nil             -- needed for recusive termination
  where canMove t = (not $ isBalanced t) && (not $ isRightFull t)

--
-- test data
--

x = 'x'
e = Nil                                          -- h = 0
l = leaf                                         -- h = 1
tL = (Node x leaf Nil)                           -- h = 2
tR = (Node x Nil leaf)                           -- h = 2
tB = (Node x leaf leaf)

t1 = (Node x (Node x leaf Nil) leaf)             -- h = 3
t2 = (Node x (Node x Nil leaf) leaf)             -- h = 3
t3 = (Node x leaf (Node x leaf Nil))             -- h = 3
t4 = (Node x leaf (Node x Nil leaf))             -- h = 3

tx = (Node x (Node x leaf leaf) leaf)
ty = (Node x (Node x leaf leaf) (Node x leaf Nil))
tz = (Node x (Node x Nil leaf) (Node x leaf Nil))
