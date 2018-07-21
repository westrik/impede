module Impede
    ( Vector(..)
    , SceneConfig (..)
    
    , render
    ) where

import Codec.Picture

import Vector
import Ray

type Scalar = Double

data SceneConfig = SceneConfig { width :: Int
                               , height :: Int
                               , lower_left :: Vector
                               , horizontal :: Vector
                               , vertical :: Vector
                               , origin :: Vector
                               } deriving (Show)  

render :: SceneConfig -> DynamicImage
render config = ImageRGB8 (generateImage (renderPixel config) (width config) (height config))

renderPixel :: SceneConfig -> Int -> Int -> PixelRGB8
renderPixel config x y = getColour $ Ray (origin config) direction
    where direction = (lower_left config) + scale (horizontal config) u + scale (vertical config) v
          u = (fromIntegral x) / fromIntegral (width config)
          v = (fromIntegral y) / fromIntegral (height config)

getColour :: Ray -> PixelRGB8
getColour ray = if (hitSphere (Vector 0 0 (-1)) 0.5 ray)
    then PixelRGB8 255 0 0 
    else PixelRGB8 127 178 255 

hitSphere :: Vector -> Scalar -> Ray -> Bool
hitSphere centre radius ray = discriminant > 0
    where discriminant = b * b - 4 * a * c
          oc = orig ray - centre
          a = dot (dir ray) (dir ray)
          b = 2 * dot oc (dir ray)
          c = dot oc oc - radius * radius