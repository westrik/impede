module Ray where

import Vector

data Ray = Ray { orig :: Vector
               , dir :: Vector 
               } deriving Show

point :: Ray -> Double -> Vector
point ray distance = orig ray + scale (dir ray) distance