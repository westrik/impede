module Vector where

data Vector = Vector { x :: {-# UNPACK #-} !Double
                     , y :: {-# UNPACK #-} !Double
                     , z :: {-# UNPACK #-} !Double
                     } deriving (Show)

instance Num Vector where
    (+) a b = Vector (x a + x b) (y a + y b) (z a + z b) 
    (*) a b = Vector (x a * x b) (y a * y b) (z a * z b) 
    (-) a b = Vector (x a - x b) (y a - y b) (z a - z b) 
    abs a = Vector (abs $ x a) (abs $ y a) (abs $ z a)
    fromInteger a = Vector v v v where v = fromIntegral a
    signum a = 0

scale :: Vector -> Double -> Vector
scale vec distance = Vector (x vec * distance) (y vec * distance) (z vec * distance)

unit_vector :: Vector -> Vector
unit_vector a = scale a (1/len a)

squared_len :: Vector -> Double
squared_len a = (x a * x a) + (y a * y a) + (z a * z a)

len :: Vector -> Double
len = sqrt . squared_len

dot :: Vector -> Vector -> Double
dot a b = (x a * x b) + (y a * y b) + (z a * z b)

cross :: Vector -> Vector -> Vector
cross a b = Vector (y a * z b - z a * y b) (-(x a * z b - z a * x b)) (x a * y b - y a * x b)