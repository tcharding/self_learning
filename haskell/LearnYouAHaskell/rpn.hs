import Data.Char (digitToInt,isDigit)
{-
Reverse Polish Notation Calculator

input: "10 4 3 + 2 * -"
ouput: -4
-}
{-
rpn :: String -> Double
rpn xs = helper [] xs
  where helper s [] = head s
        helper (a:b:cs) (x:xs)
          | num x = helper (x:s) xs
          | otherwise = helper ((applyFn x a b) : cs) xs

type N = String                 -- "14"
type Op = String                -- "+"

applyFn :: Op -> N -> N -> N
applyFn op a b = dblToStr . (strToFn op) (strToDbl a) (strToDbl b)
-}
{-
strToDbl :: String -> Double
strToDbl [] = 0
strToDbl xs@(x:xs')
  | x == '-' = (-1) * helper xs'
  | otherwise = helper xs
  where helper xs = i + (f / (n * 10))
          where (i,f) = splitIntFraction xs
                n = nDigits f
-}
splitIntFraction :: String -> (String,String)
splitIntFraction xs = (fst t,tail (snd t))
  where t = span isDigit xs
--splitIntFraction xs = ((strToInt (fst t)) , (strToInt (tail (snd t))))
  

intFractionToDouble :: (String,String) -> Double  
intFractionToDouble (i,f) = fromIntegral (strToInt i)
                            + (fromIntegral (strToInt f))
                            / (10 ^ fromIntegral n)
  where n = length f

numDigits :: Int -> Int
numDigits 0 = 0
numDigits x = 1 + numDigits (floor $ (fromIntegral x) / 10)

strToInt :: String -> Int
strToInt [] = 0
strToInt xs@(x:xs')
  | x == '-' = (-1) * helper xs'
  | otherwise = helper xs
  where helper = foldl step 0 
          where step acc x = acc * 10 + digitToInt x
