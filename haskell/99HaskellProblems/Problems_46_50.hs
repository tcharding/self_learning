-- H-99: Ninety-Nine Haskell Problems
-- https://wiki.haskell.org/99_questions

{-
Problem 46
(**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for
logical equivalence) which succeed or fail according to the result of their
respective operations; e.g. and(A,B) will succeed, if and only if both A and B
succeed.

A logical expression in two variables can then be written as in the following
example: and(or(A,B),nand(A,B)).

Now, write a predicate table/3 which prints the truth table of a given logical
expression in two variables.
-}
and' a b = a && b
or' a b = a || b
nand a b = not a && b
nor a b = not (a || b)
xor a b = (a || b) && not (a && b)
impl a b = a
equ a b = a == b

table :: (Bool -> Bool -> Bool) -> [(Bool,Bool,Bool)]
table e = let xs = [(True,True),(True,False),(False,True),(False,False)]
               in helper e xs
                  where helper _ [] = []
                        helper e ((a,b):xs) = (a,b,(e a b)):helper e xs

{-
Problem 49
(**) Gray codes.

An n-bit Gray code is a sequence of n-bit strings constructed according to
certain rules. For example,

n = 1: C(1) = ['0','1'].  n = 2: C(2) = ['00','01','11','10'].  n = 3: C(3) =
['000','001','011','010',´110´,´111´,´101´,´100´].  Find out the construction
rules and write a predicate with the following specification:

% gray(N,C) :- C is the N-bit Gray code Can you apply the method of "result
caching" in order to make the predicate more efficient, when it is to be used
repeatedly?
-}
-- needs memoizing, see https://wiki.haskell.org/Memoization
gray :: Int -> [String]
gray 1 = ["0","1"]
gray n = (prefix '1' old) ++ (prefix '0' $ reverse old)
  where old = gray (n-1)

prefix :: Char -> [String] -> [String]
prefix _ [] = []
prefix pre (x:xs) = (pre:x):prefix pre xs

