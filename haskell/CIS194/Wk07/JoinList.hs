{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Data.Monoid
import Buffer
import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) j k = Append (tagM j <> tagM k) j k

-- get annotation at root of jl as an Int
tagI :: (Sized m, Monoid m) => JoinList m a -> Int
tagI jl = getSize (size (tagM jl))

-- get it as a monoid
tagM :: Monoid m => JoinList m a -> m
tagM Empty = mempty 
tagM (Single m _) = m
tagM (Append m _ _) = m

-- Exercise 2

indexJ :: (Sized m, Monoid m) => Int -> JoinList m a -> Maybe a
indexJ _ Empty = Nothing
indexJ _ (Single _ a) = Just a
indexJ i (Append _ l r)
  | i+1 <= tagI l = indexJ i l
  | otherwise = indexJ (i - tagI l) r

-- safe list indexing 
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
  
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl

dropJ n jl | n < 0 = jl         -- defensive

dropJ n jl
  | n >= tagI jl = Empty        -- drop everything (includes Single)

dropJ n (Append _ l r)          -- drop [in] the left branch
  | n == tagI l = r
  | n < tagI l = let new = dropJ n l
                 in Append (tagM r <> tagM new) new r
  | otherwise = dropJ (n - tagI l) r       

dropJ _ (Single _ _) = error "shouldn't get here"

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl
  | n <= 0 = Empty
  | n >= tagI jl = jl           -- includes Single

takeJ n (Append _ l r)
  | n == tagI l = l
  | n < tagI l = takeJ n l
  | otherwise = let new = takeJ (n - tagI l) r
                in Append (tagM l <> tagM new) l new 

takeJ _ (Single _ _)  = error "shouldn't get here"

-- Exercise 3

-- >>> scoreLine "yay " +++ scoreLine "haskell!"
-- Append (Score 23) (Single (Score 9) "yay ") (Single (Score 14) "haskell!")

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s 

-- Exercise 4

type Bjl = JoinList (Score, Size) String

scoreSize :: String -> (Score, Size)
scoreSize s = (scoreString s, Size 1)

ss2size :: (Score, Size) -> Int
ss2size (_,s) = getSize s

ss2score :: (Score, Size) -> Int
ss2score (s,_) = getScore s

instance Buffer Bjl where

  -- | Convert a buffer to a String.
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ l r) = toString l ++ toString r

  -- | Create a buffer from a String.
  fromString s = Single (scoreSize s) s
  
  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  line _ Empty = Nothing

  line 0 (Single _ s) = Just s
  line _ (Single _ _) = Nothing
    
  line n (Append _ l r)
    | n+1 <= tagI l = line n l
    | otherwise = line (n - tagI l) r

  -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  replaceLine n _ b | n < 0 = b -- defensize

  replaceLine 0 s Empty = fromString s
  replaceLine _ _ Empty = Empty
  
  replaceLine 0 s (Single _ _) = fromString s
  replaceLine _ _ b@(Single _ _) = b
    
  replaceLine n s (Append _ l r)
    | n+1 <= tagI l = let new = replaceLine n s l 
                      in Append (tagM new <> tagM r) new r
    | otherwise = let new = replaceLine (n - tagI l) s r
                  in Append (tagM l <> tagM new) l new

-- | Compute the number of lines in the buffer.
  numLines Empty = 0
  numLines (Single ss _) = ss2size ss 
  numLines (Append ss _ _) = ss2size ss 


-- | Compute the value of the buffer, i.e. the amount someone would
--   be paid for publishing the contents of the buffer.
  value Empty = 0
  value (Single ss _) = ss2score ss 
  value (Append ss _ _) = ss2score ss 


