-- data Cartesian = Cartesian Double Double
--                deriving (Eq,Show)

data Cartesian = Cartesian {xcord, ycord :: Double}
               deriving (Eq,Show)

data Polar = Polar {radial, angular :: Double}
             deriving (Eq,Show)

magnitude (Cartesian x1 y1) (Cartesian x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
magnitude (Polar r1 a1) (Polar r2 a2) = magnitude (polarToCartesian p) (polarToCartesian q)

polarToCartesian :: Polar -> Cartesian
polarToCartesian (Polar r a) = Cartesian (r * cos a) (r * sin a)

cartesianToPolar (Cartesian x y) = Polar (atan (x / y)) (sqrt (x^2 + y^2))

a = Cartesian 0 0
b = Cartesian 3 4
c = Cartesian 3 0

e = Polar 0 0
f = Polar 30 2
g = Polar 0 (sqrt 3)



  
