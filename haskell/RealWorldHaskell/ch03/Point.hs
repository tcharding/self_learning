-- Point Data Type
import Data.List

data Point = Pt {xcord, ycord :: Double}
           deriving (Eq,Show)

magnitude a b = sqrt ((xcord b - xcord a)^2 + (ycord b - ycord a)^2)
theta a b = atan ((ycord b - ycord a) / (xcord b - xcord a))

data Direction = Right | Straight | Left
  deriving (Show,Eq)

direction :: Point -> Point -> Point -> Direction
direction a b c
  | delta < 0 = Main.Left
  | delta > 0 = Main.Right
  | otherwise = Main.Straight
  where delta = (xcord c - xcord a) * (ycord b - ycord a) -
          (ycord c - ycord a) * (xcord b - xcord a)


turnByTurn :: [Point] -> [Direction]
turnByTurn all@(p1:p2:p3:_) = (direction p1 p2 p3) : (turnByTurn $ tail all)
turnByTurn _ = []

-- Graham Scan
--  https://en.wikipedia.org/wiki/Graham_scan
grahamScan :: [Point] -> [Point]
grahamScan ps = convexHull (grahamScanSort ps)

grahamScanSort :: [Point] -> [Point]
grahamScanSort ps = let pPs = findP ps
                        lowestY = head pPs
                        rest = tail pPs
                        cmp p q = (slope lowestY p) `compare` (slope lowestY q)
                    in lowestY : (sortBy cmp rest)

convexHull :: [Point] -> [Point]
convexHull (p1:p2:p3:ps)
  | d == Main.Right = convexHull (p1:p3:ps)
  | otherwise = p1 : (convexHull (p2:p3:ps))
  where d = direction p1 p2 p3
convexHull ps = ps

slope :: Point -> Point -> Double
slope p q = (ycord q - ycord p) / (xcord q - xcord p)

{-
Find point P, put it at head of list
 P is point with lowest y co-ordinate (and lowest x co-ordinate if needed)
-}
findP :: [Point] -> [Point]
findP [p] = [p]
findP (p:ps)
  | p `lowerY` head parsed = p:parsed
  | otherwise = (head parsed):p:(tail parsed)
  where parsed = findP ps

-- True if p has lower Y coord than q (lowest x coord if y coords are the same)
lowerY :: Point -> Point -> Bool
lowerY p q
  | ycord p < ycord q = True
  | ycord p > ycord q = False
  | xcord p < xcord q = True
  | otherwise = False

-- Test data
a = Pt 0 0
b = Pt 3 4
c = Pt 3 0

f = Pt 3 4
g = Pt (-3) 4
h = Pt (-3) (-4) -- Point P (see Graham Scan)
i = Pt 3 (-4)
