-- :set prompt "ghci> "

{-

succ 8
min 3 6
max 3 4

-}

{-
Starting Out

append to end of list: ++
ghci> [3,6,4] ++ 5
[3,6,4,5]

append to front of list:
ghci> 5:[3,6,4]
[5,3,6,4]

list position accessor !!
ghci> [6,4,2] !! 1
4

head, tail, last, init
length
null

ghci> take 3 [1,2,3,4]
[1,2,3]

ghci> drop 3 [1,2,3,4]
[4]

sum, product

ghci> 4 `elem` [1,2,3,4]
True

ghci> 4 `elem` [1,2,3,5]
False

Ranges
ghci> [2,4..20]
[2,4,6,8,10,12,14,16,18,20]
ghci> [3,6..20]
[3,6,9,12,15,18]

ghci> take 10 (cycle [1,2,3])
[1,2,3,1,2,3,1,2,3,1]
ghci> take 12 (cycle "LOL ")
"LOL LOL LOL "

ghci> take 10 (repeat 5)
[5,5,5,5,5,5,5,5,5,5]

List Comprehension
------------------


-}

doubleMe x = x + x

-- doubleUs x y = x*2 + y*2

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                      then x
                      else x*2

triple x = x * 3

