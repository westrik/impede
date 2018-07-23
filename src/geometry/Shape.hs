module Shape where

import Control.Monad
import Vector
import Ray
import Data.Maybe

--------- Shapes ---------

data Shape = ListShape (ShapeList) | SphereShape (Sphere) deriving (Show)

data ShapeList = ShapeList { shapes :: [Shape] 
                           } deriving Show

data Sphere = Sphere { centre :: {-# UNPACK #-} !Vector
                     , radius :: {-# UNPACK #-} !Double
                     } deriving Show

--------- Hit Logic ---------

data Hit = Hit { t      :: {-# UNPACK #-} !Double
               , p      :: {-# UNPACK #-} !Vector
               , normal :: {-# UNPACK #-} !Vector 
               } deriving (Show)

instance Eq Hit where
    (==) a b = t a == t b

instance Ord Hit where
    (<) a b = t a < t b
    (<=) a b = t a <= t b
    (>) a b = t a > t b

class Hittable a where
    hit :: Ray -> (Double, Double) -> a -> Maybe (Hit)

instance Hittable Shape where
    hit ray (tMin, tMax) (SphereShape shape) = hit ray (tMin, tMax) shape
    hit ray (tMin, tMax) (ListShape shape) = hit ray (tMin, tMax) shape

instance Hittable ShapeList where
    hit ray (tMin, tMax) list = if length hits > 0
        then Just (minimum hits)
        else Nothing
        where hits = catMaybes $ fmap (hit ray (tMin, tMax)) (shapes list)
        
instance Hittable Sphere where
    hit ray (tMin, tMax) sph = if discriminant > 0
        then msum [(calcHit $ ((-b) - (sqrt $ b * b - a * c)) / a), 
                    (calcHit $ ((-b) + (sqrt $ b * b - a * c)) / a)]
        else Nothing
        where discriminant = b * b - a * c
              oc = orig ray - centre sph
              a = dot (dir ray) (dir ray)
              b = dot oc (dir ray)
              c = dot oc oc - (radius sph) * (radius sph)
              calcHit t = if t < tMax && t > tMin 
                          then Just (Hit t p (scale (p - centre sph) (1 / radius sph)))
                          else Nothing
                          where p = point ray t