{-# LANGUAGE TypeOperators #-}

module Impede
    ( Vector (..)
    , SceneConfig (..)
    
    , render
    ) where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import Data.Array.Repa (Array, DIM1, DIM2, D, Z (..), (:.)(..), (!))
import qualified Codec.Picture.Types as M
import qualified Data.Array.Repa     as R
import Data.Array.Repa.Repr.Vector

import Vector
import Ray
import Colour

data SceneConfig = SceneConfig { width :: Int
                               , height :: Int
                               , lowerLeft :: Vector
                               , horizontal :: Vector
                               , vertical :: Vector
                               , origin :: Vector
                               } deriving (Show)  

render :: SceneConfig -> IO (DynamicImage)
render conf = do
    renderedScene <- R.computeP $ renderScene conf
    return $ ImageRGB8 $ toImage renderedScene

renderScene :: SceneConfig -> Array D DIM2 Colour
renderScene conf = R.fromFunction (Z :. width conf :. height conf) (renderPixel conf)

renderPixel :: SceneConfig -> DIM2 -> Colour
renderPixel conf (Z :. i :. j) = getColour $ Ray (origin conf) direction
    where direction = lowerLeft conf + scale (horizontal conf) u + scale (vertical conf) v
          u = fromIntegral i / fromIntegral (width conf)
          v = fromIntegral j / fromIntegral (height conf)

toImage :: Array V DIM2 Colour -> Image PixelRGB8
toImage a = generateImage gen width height
    where Z :. width :. height = R.extent a
          gen x y = pixel $ a ! (Z :. x :. y)
          {-# INLINE gen #-}

getColour :: Ray -> Colour
getColour ray = if hitSphere (Vector 0 0 (-1)) 0.5 ray 
    then Colour 1 0 0 
    else scaleColour (Colour 1 1 1) (1 - t) + scaleColour (Colour 0.5 0.7 1) t
    where t = 0.5 * (y unitDir + 1)
          unitDir = unitVector (dir ray)

hitSphere :: Vector -> Double -> Ray -> Bool
hitSphere centre radius ray = discriminant > 0
    where discriminant = b * b - 4 * a * c
          oc = orig ray - centre
          a = dot (dir ray) (dir ray)
          b = 2 * dot oc (dir ray)
          c = dot oc oc - radius * radius
