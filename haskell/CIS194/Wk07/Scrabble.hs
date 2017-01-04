{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

--import Data.Monoid

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

score :: Char -> Score
score c
  | elem c ['e','a','i','o','n','r','t','l','s','u'] = Score 1
  | elem c ['d','g'] = Score 2
  | elem c ['b','c','m','p'] = Score 3
  | elem c ['f','h','v','w','y'] = Score 4
  | elem c ['k'] = Score 5
  | elem c ['j','x'] = Score 8
  | elem c ['q','z'] = Score 10
  | otherwise = Score 0

scoreString :: String -> Score
scoreString = foldr f (Score 0)
  where f c acc = acc + score c 

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

