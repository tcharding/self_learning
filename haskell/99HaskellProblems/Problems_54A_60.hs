import Data.List
import BST

-- H-99: Ninety-Nine Haskell Problems
-- https://wiki.haskell.org/99_questions

-- miscellanious tree functions

-- value held in tree not important to these problems
type T = Tree Char              
leaf = Node 'x' Nil Nil

-- i.e size [1,3,7,15]
isFull :: Tree a -> Bool
isFull Nil = True
isFull (Node _ l r) = isFull l && isFull r && (height l == height r)

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

-- generate a complete binare tree with n nodes 
cTree :: Int -> T
cTree 0 = Nil
cTree n = addNode (cTree (n-1))

{-
-- add node to the right of last node
addNodeRH :: T -> T
addNodeRH t
  | isFull t = addNodeL t
  | otherwise = helper t
    where helper Nil = leaf
          helper (Node v l@(Node _ Nil Nil) Nil) = Node v l leaf
          helper (Node v l r)
            | isFull l && isFull r = Node v l (addNodeL r) -- l(h+1), r(h)
            | hasSingleLeftChild l = Node v (helper l) r
            | isRightFull l && isFull r = Node v l (addNodeL r)
            | otherwise = Node v l (helper r)

-- Tree contains a subtree with a left child and no right child
hasSingleLeftChild :: Tree a -> Bool
hasSingleLeftChild (Node _ l Nil) = True
hasSingleLeftChild (Node _ l r) = hasSingleLeftChild l || hasSingleLeftChild r
hasSingleLeftChild _ = False

-- true if right most position of tree contains a leaf
isRightFull :: T -> Bool
isRightFull t
  | isFull t = True
isRightFull (Node _ Nil Nil) = True -- base case at far right of tree
isRightFull (Node _ l r) = (height l < height r) && isRightFull r
-}

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
{-
cBalTree :: (Num t, Eq t) => Int -> [T]
cBalTree 1 = [leaf]
cBalTree n
  | fillsTree n = [cTree n]     -- base case is full tree depth h-1
  | otherwise = filter isCBal $ nub . concat . map cultivate $ cBalTree (n-1)

-- true if tree of size n is 'full' i.e n = 2**H - 1
fillsTree :: Int -> Bool
fillsTree n = helper n [floor $ 2**h - 1 | h <- [1..]]
  where helper n (x:xs)
          | n < x = False
          | otherwise = n == x || helper n xs

-- completely balanced tree
isCBal :: Tree a -> Bool
isCBal Nil = True
isCBal (Node _ l r) = isCBal l && isCBal r && (size l `almostEq` size r)

-- inputs differ by at most 1
almostEq :: Int -> Int -> Bool
almostEq a b = abs (a - b) <= 1

-- build list of all balanced trees by adding node to tree
cultivate :: T -> [T]
cultivate t
  | isFull t = helper (addNodeL t)
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
  where canMove t = (not $ isFull t) && (not $ isRightFull t)
-}
--
-- test data
--

x = 'x'
e = Nil                                          -- h = 0
l = leaf                                         -- h = 1
tL = (Node x leaf Nil)                           -- h = 2
tR = (Node x Nil leaf)                           -- h = 2
tF = (Node x leaf leaf)

t1 = (Node x (Node x leaf Nil) leaf)             -- h = 3
t2 = (Node x (Node x Nil leaf) leaf)             -- h = 3
t3 = (Node x leaf (Node x leaf Nil))             -- h = 3
t4 = (Node x leaf (Node x Nil leaf))             -- h = 3

tx = (Node x (Node x leaf leaf) leaf)
ty = (Node x (Node x leaf leaf) (Node x leaf Nil))
tz = (Node x (Node x Nil leaf) (Node x leaf Nil))

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

mirror :: Eq a => Tree a -> Tree a -> Bool
mirror t1 t2 = t1 `structureEq` reverseTree t2
  where reverseTree (Node v l r) = (Node v (reverseTree r) (reverseTree l))
        reverseTree Nil = Nil

structureEq :: Tree a -> Tree a -> Bool
structureEq Nil Nil = True
structureEq t Nil = False
structureEq Nil t = False
structureEq (Node _ Nil Nil) (Node _ Nil Nil) = True
structureEq (Node _ Nil r1) (Node _ Nil r2) = structureEq r1 r2
structureEq (Node _ l1 Nil) (Node _ l2 Nil) = structureEq l1 l2
structureEq (Node _ l1 r1) (Node _ l2 r2) = (structureEq l1 l2) && (structureEq r1 r2)

{-
Problem 57
(**) Binary search trees (dictionaries)

Use the predicate add/3, developed in chapter 4 of the course, to write a
predicate to construct a binary search tree from a list of integer numbers.
-}

buildTree :: Ord a => [a] -> Tree a
buildTree xs = addAlt Nil (reverse front) back
  where (front,back) = splitInHalf $ sort xs

addAlt :: Ord a => Tree a -> [a] -> [a] -> Tree a
addAlt t (x:xs) (y:ys) = addAlt (add (add t x) y) xs ys
addAlt t [] [y] = add t y
addAlt t [x] [] = add t x
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
{-
symCBalTree :: Int -> [T]
symCBalTree n = filter symmetric $ cBalTree n
-}
{-
Problem 59
(**) Construct height-balanced binary trees

In a height-balanced binary tree, the following property holds for every node:
The height of its left subtree and the height of its right subtree are almost
equal, which means their difference is not greater than one.

Construct a list of all height-balanced binary trees with the given element and
the given maximum height.
-}



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
{-
hBalTree :: (Num t, Eq t) => Int -> [T]
hBalTree 1 = [leaf]
hBalTree n = nub . concat . map cultivate $ hBalTree (n-1)
-}
