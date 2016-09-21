{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
-- Exercise 1

-- the nth Fibonacci number
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

-- the infinite Fibonacci list
fibs1 :: [Integer]
fibs1 = [fib x | x <- [0..]]

-- Exercise 2

-- Big O(n)
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)
--fibs2 = zipWith (+) fibs1 (tail fibs1)

-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

instance Show a => Show (Stream a) where
  show = go (0 :: Int)
    where go 20 _ = ""
          go c (Cons x s) = show x ++ " : " ++ go (c+1) s

-- Exercise 4

-- stream of identical elements
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- map f over stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) (streamMap f s)

-- generate stream from seed and transform function
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Exercise 5

-- stream of natural numbers
nats :: Stream Integer
nats = streamFromSeed (+1) 1

-- the ruler function
-- https://oeis.org/A007814
ruler :: Stream Integer
ruler = go (1 :: Int)
  where go n = Cons (f n) (go (n+1))
        f n
          | odd n = 0
          | otherwise = 1 + (f (n `div` 2))

-- https://oeis.org/A001511
trueRuler :: Stream Integer
trueRuler = streamMap (+1) ruler

-- Exercise 6

xS :: Stream Integer
xS = Cons 0 (Cons 1 (streamRepeat 0))

-- >>> xS^4
-- >>> (1 + xS)^5
-- >>> (xS^2 + xS + 3) * (xS - 5)

instance Num (Stream Integer) where
  fromInteger x = Cons x (streamRepeat 0)
  negate (Cons x s) = Cons (negate x) (negate s)

  (+) (Cons x s) (Cons y t) = Cons (x + y) (s + t)
  (*) (Cons x s) t@(Cons y u) = Cons (x * y) ((streamMap (*x) u) + (s * t))

  abs = undefined
  signum = undefined

instance Fractional (Stream Integer) where
  (/) (Cons x s) (Cons y t) = q
    where q = Cons (x `div` y) (streamMap (*x) (streamMap (*(1 `div` y)) (s - (q * t))))

  fromRational = undefined

fibs3 :: Stream Integer
--fibs3 = xS / (streamMap (1-) (xS + (xS * xS)))
fibs3 = undefined               -- did not get this one!

-- Exercise 7

-- Matrix 1 2 3 4 =
-- -    -
-- | 1 2 |
-- | 3 4 |
-- -    -
data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  (*) (Matrix a b c d) (Matrix w x y z) = Matrix (a*x + b*y) (a*x + b*z) (c*w + d*y) (c*x + d*z)
  (+) = undefined
  (-) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined

-- fib n in Big O(log n)
fib4 :: Integer -> Integer
fib4 n = matrix2fib (fibMatrix ^ n)
  where fibMatrix = Matrix 1 1 1 0

-- get fibonacci number from matrix
matrix2fib :: Matrix -> Integer
matrix2fib (Matrix f _ _ _) = f
