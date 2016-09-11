module Prettify where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
  deriving (Show,Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double n = text (show n)

line :: Doc
line = Line


(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat =  fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

compact :: Doc -> String
compact x = transfrom [x]
  where transfrom [] = ""
        transfrom (d:ds) =
          case d of
            Empty -> transfrom ds
            Char c -> c : transfrom ds
            Text s -> s ++ transfrom ds
            Line -> '\n' : transfrom ds
            x `Concat` y -> transfrom (x:y:ds)
            _ `Union` y -> transfrom (y:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where best col (d:ds) =
          case d of
            Empty -> best col ds
            Char c -> c : best (col + 1) ds
            Text s -> s ++ best (col + length s) ds
            Line -> '\n' : best 0 ds
            x `Concat` y -> best col (x:y:ds)
            x `Union` y -> nicest col (best col (x:ds))
                                      (best col (y:ds))
        best _ _ = ""

        nicest col x y | (width - least) `fits` x = x
                       | otherwise = y
          where least = min width col
        
fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` "" = True
w `fits` ('\n':_) = True
w `fits` (c:cs) = (w - 1) `fits` cs

fill :: Int -> Doc -> Doc
fill col d
  | col <= width = d
  | otherwise = text (replicate n ' ') <> d
    where width = length (compact d)
          n = col - width
