{-# OPTIONS_GHC -Wall #-}

-- validating credit card numbers

{- toDigits 1234 == [1,2,3,4]
   toDigitsRev 1234 == [4,3,2,1]
   toDigits 0 == []
   toDigits (-17) == [] -}

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = r : toDigitsRev q
    where (q,r) = quotRem n 10  
          
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

{- doubleEveryOther [8,7,6,5] == [16,7,12,5]
   doubleEveryOther [1,2,3] == [1,4,3] -}

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . helper . reverse
  where helper [] = []
        helper [x] = [x]
        helper (x:y:xs) = x : (y*2) : helper xs


{- sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22 -}

sumDigits :: [Integer] -> Integer
-- foldl ?
sumDigits = helper 0
  where helper s [] = s
        helper s (x:xs) = helper (s + (sum $ toDigits x)) xs
        
{- validate 4012888888881881 = True
   validate 4012888888881882 = False -}

validate :: Integer -> Bool
validate n = r == 0
  where r = (sumDigits $ doubleEveryOther $ toDigits n) `rem` 10

-- the towers of Hanoi
-- pegs: a b c

type Peg = String
type Move = (Peg, Peg)

{- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")] -}

-- move discs from a to b using c as intermediary
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a,b)]
hanoi n a b c = hanoi (n-1) a c b ++ (a,b) : hanoi (n-1) c b a
