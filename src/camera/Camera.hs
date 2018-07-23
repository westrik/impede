module Camera where

import Vector
import Ray
    
data Camera = Camera { lowerLeft :: Vector
                     , horizontal :: Vector
                     , vertical :: Vector
                     , origin :: Vector    
                     } deriving Show

getRay :: Camera -> Double -> Double -> Ray
getRay cam u v = Ray (origin cam) 
                   $ lowerLeft cam + scale (horizontal cam) u + scale (vertical cam) v
{-# INLINE getRay #-}