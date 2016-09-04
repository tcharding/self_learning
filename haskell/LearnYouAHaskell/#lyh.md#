Learn You a Haskell for Great Good
==================================

Starting Out
============

* append to end of list: ++

ghci> [3,6,4] ++ 5
[3,6,4,5]

* append to front of list: :
ghci> 5:[3,6,4]
[5,3,6,4]

* list position accessor !!
ghci> [6,4,2] !! 1
4

ghci> take 3 [1,2,3,4]
[1,2,3]

ghci> drop 3 [1,2,3,4]
[4]

ghci> 4 `elem` [1,2,3,4]
True

ghci> 4 `elem` [1,2,3,5]
False

Ranges
------
ghci> [2,4..20]
[2,4,6,8,10,12,14,16,18,20]

ghci> [3,6..20]
[3,6,9,12,15,18]


functions
---------
head, tail, last, init
length
null
sum, product
take, drop
elem
show

ghci> show True
"True"

ghci> take 10 (cycle [1,2,3])
[1,2,3,1,2,3,1,2,3,1]

ghci> take 12 (cycle "LOL ")
"LOL LOL LOL "

ghci> take 10 (repeat 5)
[5,5,5,5,5,5,5,5,5,5]

List Comprehension
------------------
ghci> [x*2 | x <- [1..10]]
[2,4,6,8,10,12,14,16,18,20]

ghci> [x*2 | x <- [1..10], x*2 >= 12]
[12,14,16,18,20]

ghci> [ x | x <- [50..100], x `mod` 7 == 3]
[52,59,66,73,80,87,94]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
ghci> boomBangs [7..13]
["BOOM!","BOOM!","BANG!","BANG!"]

ghci> [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
[10,11,12,14,16,17,18,20]

ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]
[16,20,22,40,50,55,80,100,110]

ghci> [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
[55,80,100,110]

ghci> let nouns = ["hobo","frog","pope"]
ghci> let adjectives = ["lazy","grouchy","scheming"]
ghci> [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]
["lazy hobo","lazy frog","lazy pope","grouchy hobo","grouchy frog",
"grouchy pope","scheming hobo","scheming frog","scheming pope"]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

ghci> removeNonUppercase "Hahaha! Ahahaha!"
"HA"
ghci> removeNonUppercase "IdontLIKEFROGS"
"ILIKEFROGS"

ghci> let xxs =
[[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]

ghci> [ [ x | x <- xs, even x ] | xs <- xxs]
[[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]

Tuples
------

fst, snd

ghci> fst (8,11)
8
ghci> fst ("Wow", False)
"Wow"

ghci> snd (8,11)
11
ghci> snd ("Wow", False)
False

ghci> zip [1,2,3,4,5] [5,5,5,5,5]
[(1,5),(2,5),(3,5),(4,5),(5,5)]
ghci> zip [1 .. 5] ["one", "two", "three", "four", "five"]
[(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")]

ghci> zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]
[(5,"im"),(3,"a"),(2,"turtle")]

ghci> zip [1..] ["apple", "orange", "cherry", "mango"]
[(1,"apple"),(2,"orange"),(3,"cherry"),(4,"mango")]

ghci> let rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b],
a^2 + b^2 == c^2, a+b+c == 24]
ghci> rightTriangles'
[(6,8,10)]

Types And Typeclasses
=====================
Int, Integer, Float, Double, Bool, Char, ()

Eq, Ord, Show, Enum, Bounded, Num, Integral, Floating

ghci> read "5" :: Int
5
ghci> read "5" :: Float
5.0

Syntax and Functions
====================

