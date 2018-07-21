module Impede
    ( Vector (..)
    , SceneConfig (..)
    
    , render
    ) where

import Codec.Picture

import Vector
import Ray

data SceneConfig = SceneConfig { width :: Int
                               , height :: Int
                               , lowerLeft :: Vector
                               , horizontal :: Vector
                               , vertical :: Vector
                               , origin :: Vector
                               } deriving (Show)  

render :: SceneConfig -> DynamicImage
render conf = ImageRGB8 (generateImage (renderPixel conf) (width conf) (height conf))

-- TODO: rewrite this to allocate a matrix then run rendering in parallel on the matrix

-- renderScene :: SceneConfig -> REPA Colour
-- render config = parallel_map (renderPixel config) (REPA (width config) (height config))

-- getPixel :: REPA Colour -> Int -> Int -> PixelRGB8

renderPixel :: SceneConfig -> Int -> Int -> PixelRGB8
renderPixel conf i j = getColour $ Ray (origin conf) direction
    where direction = lowerLeft conf + scale (horizontal conf) u + scale (vertical conf) v
          u = fromIntegral i / fromIntegral (width conf)
          v = fromIntegral j / fromIntegral (height conf)

getColour :: Ray -> PixelRGB8
getColour ray = if hitSphere (Vector 0 0 (-1)) 0.5 ray 
                    then PixelRGB8 255 0 0 
                    else normal
    where normal = PixelRGB8 127 178 255 -- (1 - t) * PixelRGB8 255 255 255 + t * PixelRGB8 127 178 255 
          -- unitDir = unitVector (dir ray)
          -- t = 0.5 * (y unitDir + 1)

hitSphere :: Vector -> Double -> Ray -> Bool
hitSphere centre radius ray = discriminant > 0
    where discriminant = b * b - 4 * a * c
          oc = orig ray - centre
          a = dot (dir ray) (dir ray)
          b = 2 * dot oc (dir ray)
          c = dot oc oc - radius * radius