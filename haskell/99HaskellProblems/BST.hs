module BST where

import Data.List

{-
 Binary Search Tree 
-}

data Tree a = Node a (Tree a) (Tree a)
            | Nil
              deriving (Eq)

instance Show a => Show (Tree a)
  where show Nil = "Nil"
        show (Node x Nil Nil) = show x
        show (Node x l r) = "([" ++ show x ++ "] " ++ show l ++ " " ++ show r ++ ")"

height :: Tree a -> Int
height Nil = 0
height (Node _ l r) = 1 + max (height l) (height r)

size :: Tree a -> Int
size Nil = 0
size (Node _ l r) = 1 + (size l) + (size r)

minT :: Ord a => Tree a -> a
minT (Node v Nil Nil) = v
minT (Node v Nil _) = v
minT (Node v l r) = minT l

maxT :: Tree a -> a
maxT (Node v Nil Nil) = v
maxT (Node v _ Nil) = v
maxT (Node v l r) = maxT r

preOrder :: Tree a -> [a]
preOrder (Node v l r) = [v] ++ inOrder l ++ inOrder r
preOrder _ = []

inOrder :: Tree a -> [a]
inOrder (Node v l r) = inOrder l ++ [v] ++ inOrder r
inOrder _ = []

postOrder :: Tree a -> [a]
postOrder (Node v l r) = inOrder l ++ inOrder r ++ [v]
postOrder _ = []

search :: (Ord a,Eq a) => a -> Tree a -> Bool
search x Nil = False
search x (Node v l r)
  | x == v = True
  | x < v = search x l
  | otherwise = search x r

{-
can we get these in a funcitonal implementation?
  i.e with out a parent pointer
predecessor :: Tree a -> Maybe a
predecessor (Node _ (Node v _ _) _) = Just v 
predecessor _ = undefined

successor :: Ord a => Tree a -> Maybe a
successor (Node _ l r) = Just (minT r)
successor _ = undefined
-}

-- supports multiple nodes of same value
add :: Ord a => Tree a -> a -> Tree a
add Nil x = (Node x Nil Nil)
add (Node v l r) x
  | x < v = Node v (add l x) r
  | otherwise = Node v l (add r x)

-- ref: Introduction to Algorithms
--  Cormen, Leiserson, Rivest, Stein
deleteT :: (Eq a,Ord a) => a -> Tree a -> Tree a
--deleteT x Nil = Nil
--deleteT x (Node v Nil Nil) = Nil
deleteT x (Node v Nil r)
  | x == v = r
deleteT x (Node v l Nil)
  | x == v = l
deleteT x (Node v l (Node w Nil r))
  | x == v = Node w l r
deleteT x (Node v l (Node a (Node b Nil r) s))
  | x == v = Node b l (Node a r s)
deleteT x (Node v l r)
  | x < v = Node v (deleteT x l) r
  | otherwise = Node v l (deleteT x r)

-- naive implementation
buildT :: Ord a => [a] -> Tree a
buildT = naive Nil
  where naive t (x:xs) = naive (add t x) xs
        naive t [] = t

{-
 Rudementry testing by someone who hasn't yet learnt monads
  or Haskell test frameworks
-}

t = buildT [3,5,2,4,1]

{-
tests = [testMinT]

test [] = "ok"
test (t:ts) = 
  if snd (t) == False
    then "Test failed: " ++ fst t
    else test ts

testMinT = ("MinT", minT t == 1)


test = do
  testMinT
  testMaxT

testMinT =
  if (minT t) /= 1
    then tf "minT"
    else ""   

testMaxT =
  if (maxT t) /= 5
    then tf "maxT"
    else ""   

testHeight =
  if (height t) /= 3
  then tf "height"
  else ""

testSize =
  if (size t) /= 3
  then tf "size"
  else ""
  
tf msg = error ("test failed: " ++ msg)
-}
