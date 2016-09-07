import Data.List

data Tree t = Leaf Char Weight
            | Branch Weight (Tree t) (Tree t)
            deriving Show

type Weight = Integer

huffman :: [(Char,Weight)] -> [(Char,String)]
huffman = reverse . flatten . huffmanTree . symbolsToLeaves . sortSymbols 

flatten :: Tree t -> [(Char,String)]
flatten = helper ""
  where helper acc (Leaf c _) = [(c,acc)]
        helper acc (Branch _ l r) = helper ('0':acc) l ++ helper ('1':acc) r

-- tree's are sorted in increasing order
huffmanTree :: [Tree t] -> Tree t
huffmanTree [t] = t
huffmanTree (t:u:ts) = huffmanTree (insertBranch (makeBranch u t) ts)

symbolsToLeaves :: [(Char,Weight)] -> [Tree t]
symbolsToLeaves ((c,w):xs) = (Leaf c w):symbolsToLeaves xs
symbolsToLeaves _ = []

sortSymbols :: [(Char,Weight)] -> [(Char,Weight)]
sortSymbols = sortBy cmp
  where cmp (_,w) (_,v) = w `compare` v

insertBranch :: Tree t -> [Tree t] -> [Tree t]
insertBranch x [] = [x]
insertBranch b@(Branch w _ _) (x@(Leaf _ v):xs)
  | w < v = b:x:xs             
  | otherwise = x:insertBranch b xs
insertBranch b@(Branch w _ _) (x@(Branch v _ _):xs)
  | w < v = b:x:xs             
  | otherwise = x:insertBranch b xs

makeBranch :: Tree t -> Tree t -> Tree t
makeBranch t@(Leaf _ w) u@(Branch v _ _) = Branch (w+v) t u
makeBranch t@(Leaf _ w) u@(Leaf _ v) = Branch (w+v) t u
makeBranch t@(Branch w _ _) u@(Branch v _ _) = Branch (w+v) t u
makeBranch t@(Branch w _ _) u@(Leaf _ v) = Branch (w+v) t u


-- test data
t = Branch 15 (Leaf 'a' 3) (Leaf 'b' 6)
l = Leaf 'a' 10
s = [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
