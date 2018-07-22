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
          gen x y = pixel $ a ! (Z :. x :. (height - y- 1))
          {-# INLINE gen #-}

getColour :: Ray -> Colour
getColour ray = if t > 0
    then scaleColour (Colour (x n + 1) (y n + 1) (z n + 1)) 0.5
    else scaleColour (Colour 1 1 1) (1 - t_) + scaleColour (Colour 0.5 0.7 1) t_
    where t = hitSphere (Vector 0 0 (-1)) 0.5 ray 
          n = unitVector (point ray t - Vector 0 0 (-1))
          t_ = 0.5 * (y unitDir + 1)
          unitDir = unitVector (dir ray)

hitSphere :: Vector -> Double -> Ray -> Double
hitSphere centre radius ray = if discriminant < 0
    then -1 
    else ((-b) - sqrt(discriminant)) / (2 * a)
    where discriminant = b * b - 4 * a * c
          oc = orig ray - centre
          a = dot (dir ray) (dir ray)
          b = 2 * dot oc (dir ray)
          c = dot oc oc - radius * radius
