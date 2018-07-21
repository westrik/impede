{-# LANGUAGE PatternSynonyms #-}

module Impede
    ( Vector(..)
    , SceneConfig (..)
    
    , render
    ) where

import Codec.Picture

import Vector
import Ray

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
renderPixel config x y = getColour(Ray{o = Vector(0, 0, 0), d = Vector(0, 0, 0)})

getColour :: Ray -> PixelRGB8
getColour ray = PixelRGB8 127 178 255 
