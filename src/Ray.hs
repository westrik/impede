module Ray where

import Vector

data Ray = Ray { o :: Vector
               , d :: Vector
               } deriving (Show)  