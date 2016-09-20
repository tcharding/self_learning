{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

import qualified Data.Map as M
import Data.List (find)
import ExprT
-- data ExprT = Lit Integer
--            | Add ExprT ExprT
--            | Mul ExprT ExprT
--   deriving (Show, Eq)

-- 'parser' only handles expressions with two operators
import Parser 
-- Calc> parseExp Lit Add Mul "(2+3)*4"
-- Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
--
-- Calc> parseExp Lit Add Mul "2+3*4"
-- Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))
--
-- Calc> parseExp Lit Add Mul "2+3*"
-- Nothing
import StackVM

-- Exercise 1

-- >>> eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
-- 20
eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add e f) = (+) (eval e) (eval f)
eval (ExprT.Mul e f) = (*) (eval e) (eval f)

-- Exercise 2 

-- parseExp appears to only function with two operators
evalStr :: String -> Maybe Integer
evalStr s = evalParsed (parseString s)

-- evaluate result returned by parseExp
evalParsed :: Maybe ExprT -> Maybe Integer
evalParsed (Just e) = Just (eval e)
evalParsed Nothing = Nothing

parseString :: String -> Maybe ExprT
parseString s
  | wellFormed s = parseExp ExprT.Lit (fstCon s) (sndCon s) s
  | otherwise = Nothing
  where wellFormed s' = length (opsOnly s') == 2 -- parseExp only handles this case
        fstCon = op2con . fstOp
        sndCon = op2con . sndOp

-- horrible code
fstOp :: String -> Char
fstOp s = head $ opsOnly s

-- horrible code
sndOp :: String -> Char
sndOp s = head $ tail $ opsOnly s

opsOnly :: String -> String
opsOnly = filter p
  where p x = x == '+' || x == '*'

op :: String -> Maybe Char
op = find p
  where p x = x == '+' || x == '*'
  
op2con :: Char -> ExprT -> ExprT -> ExprT
op2con '+' = ExprT.Add
op2con '*' = ExprT.Mul
op2con _ = error "operator not supported"

-- Exercise 3

-- >>> mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
-- Mul (Add (Lit 2) (Lit 3)) (Lit 4)

class Expr e where
  lit :: Integer -> e
  mul :: e -> e -> e
  add :: e -> e -> e

instance Expr ExprT where
  lit x = ExprT.Lit x
  add e f = ExprT.Add e f
  mul e f = ExprT.Mul e f

-- constrain the type of the argument to Exprt
reify :: ExprT -> ExprT
reify = id

-- Exercise 4

instance Expr Integer where
  lit x = x
  add e f = e + f
  mul e f = e * f

instance Expr Bool where
  lit x
    | x <= 0 = False
    | otherwise = True
  add e f = e || f
  mul e f = e && f

newtype MinMax = MinMax Integer
  deriving (Eq, Show, Ord)

instance Expr MinMax where
  lit x = MinMax x
  add e f = max e f
  mul e f = min e f
  
newtype Mod7 = Mod7 Integer
  deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool
-- testMM = testExp :: Maybe MinMax
-- testSat = testExp :: Maybe Mod7

-- Exercise 5

instance Expr Program where
  lit x = [PushI x]
  add e f = e ++ f ++ [StackVM.Add]
  mul e f = e ++ f ++ [StackVM.Mul]

testProgram :: Maybe Program
testProgram = testExp :: Maybe Program

compile :: String -> Maybe Program
compile s = parseExp lit (fstOpW s) (sndOpW s) s
  where fstOpW = op2word . fstOp
        sndOpW = op2word . sndOp
  
op2word :: Expr e => Char -> e -> e -> e
op2word '+' = add
op2word '*' = mul
op2word _ = error "operator not supported"

-- Exercise 6

withVars :: [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs ex = ex $ M.fromList vs 

-- >>> :t add (lit 3) (var "x")
-- add (lit 3) (var "x") :: (Expr a, HasVars a) => a
--
-- >>> withVars [("x", 6)] $ add (lit 3) (var "x")
-- Just 9
--
-- >>> withVars [("x", 6)] $ add (lit 3) (var "y")
-- Nothing
--
-- >>> withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
-- Just 54

class HasVars a where
  var :: String -> a

-- >>> add (lit 3) (var "x") :: VarExprT

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance HasVars VarExprT where
  var s = Var s

instance Expr VarExprT where
  lit x = Main.Lit x
  add e f = Main.Add e f
  mul e f = Main.Mul e f

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var s = M.lookup s

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x _ = Just x
  add e f m = addM (e m) (f m)
  mul e f m = mulM (e m) (f m)

-- add Maybe Integers together
addM :: Maybe Integer -> Maybe Integer -> Maybe Integer
addM (Just x) (Just y) = Just (x + y)
addM _ _ = Nothing

-- multiply Maybe Integers together
mulM :: Maybe Integer -> Maybe Integer -> Maybe Integer
mulM (Just x) (Just y) = Just (x * y)
mulM _ _ = Nothing
