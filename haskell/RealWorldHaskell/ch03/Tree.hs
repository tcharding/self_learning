data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ left right) = 1 + (max (treeHeight left) (treeHeight right))

simpleTree = Node "parent" (Node "left child" Empty Empty) (Node "right child" Empty Empty)
