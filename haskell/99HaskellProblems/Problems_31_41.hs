import Data.Numbers.Primes
import Data.List (group)

-- H-99: Ninety-Nine Haskell Problems
-- https://wiki.haskell.org/99_questions

{-
Problem 31
(**) Determine whether a given integer number is prime.
-}
isPrime :: Int -> Bool
isPrime n = not $ and [n `mod` x == 0 | x <- [2..(floor $ sqrt $ fromIntegral n)]]

{-
Problem 32 (**) Determine the greatest common divisor of two positive integer
numbers. Use Euclid's algorithm.
-}
gcd' :: Int -> Int -> Int
gcd' a b
  | b == 0 = a
  | otherwise = gcd' b (a `mod` b)

{-
Problem 33
(*) Determine whether two positive integer numbers are coprime. Two numbers are
coprime if their greatest common divisor equals 1.
-}  
areCoprime :: Int -> Int -> Bool
areCoprime a b = gcd' a b == 1

{-
Problem 34
(**) Calculate Euler's totient function phi(m).

Euler's so-called totient function phi(m) is defined as the number of positive
integers r (1 <= r < m) that are coprime to m.

Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) =
1.
-}
totient :: Int -> Int
totient m = length $ filter id (map (areCoprime m) [1..(m-1)])

{-
Problem 35
(**) Determine the prime factors of a given positive integer. Construct a flat
list containing the prime factors in ascending order.
-}

-- library funciton Data.Numbers.Primes.primeFactors

{-
Problem 36
(**) Determine the prime factors of a given positive integer.

Construct a list containing the prime factors and their multiplicity.
-}
pfmp :: Int -> [(Int,Int)]
pfmp = multiplicityPairs . primeFactors

multiplicityPairs :: [Int] -> [(Int,Int)]
multiplicityPairs = helper . group
  where helper [] = []
        helper (x:xs) = ((head x),(length x)) : helper xs
  
{-
Problem 37
(**) Calculate Euler's totient function phi(m) (improved).

See problem 34 for the definition of Euler's totient function. If the list of
the prime factors of a number m is known in the form of problem 36 then the
function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2)
(p3 m3) ...) be the list of prime factors (and their multiplicities) of a given
number m. Then phi(m) can be calculated with the following formula:

phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
         (p2 - 1) * p2 ** (m2 - 1) * 
         (p3 - 1) * p3 ** (m3 - 1) * ...

Note that a ** b stands for the b'th power of a.
-}
{-
-- ghc error: No instance for (Floating Int) arising from a use of ‘step
phi :: Int -> Int
phi = foldl step 1 . pfmp
  where step acc (p,m) = floor $ (p-1) * (p**(m-1)) * acc
-}

{-
Problem 39
(*) A list of prime numbers.

Given a range of integers by its lower and upper limit, construct a list of all
prime numbers in that range.
-}
primesR :: Int -> Int -> [Int]
primesR l u = takeWhile (<u) (dropWhile (<l) primes)

{-
Problem 40
(**) Goldbach's conjecture.

Goldbach's conjecture says that every positive even number greater than 2 is the
sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous
facts in number theory that has not been proved to be correct in the general
case. It has been numerically confirmed up to very large numbers (much larger
than we can go with our Prolog system). Write a predicate to find the two prime
numbers that sum up to a given even integer.
-}
goldbach :: Int -> (Int,Int)
goldbach n = helper gs
  where helper [] = (0,0)       -- protect call to head
        helper gs = head gs
        gs = [(x,y) | x <- ps, y <- ps, x + y == n]
          where ps = takeWhile (<n) primes

{-
Problem 41
(**) Given a range of integers by its lower and upper limit, print a
list of all even numbers and their Goldbach composition.

In most cases, if an even number is written as the sum of two prime numbers, one
of them is very small. Very rarely, the primes are both bigger than say 50. Try
to find out how many such cases there are in the range 2..3000.
-}

lgoldbach :: Int -> Int -> [(Int,Int,Int)]
lgoldbach l u = helper [] (filter even [l..u])
  where helper acc [] = acc
        helper acc (x:xs) = helper ((x,fst g,snd g):acc) xs
          where g = goldbach x

test = lgoldbach 2 20

nGreaterThan50 = length $ filter p (lgoldbach 2 3000)
  where p (_,x,y) = x > 50 && y > 50 

