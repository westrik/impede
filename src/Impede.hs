{-# LANGUAGE TypeOperators #-}

module Impede
    ( Vector (..)
    , SceneConfig (..)
    , Shape (..)
    , ShapeList (..)
    , Sphere (..)
    
    , render
    ) where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import Data.Array.Repa (Array, DIM1, DIM2, D, Z (..), (:.)(..), (!))
import qualified Codec.Picture.Types as M
import qualified Data.Array.Repa     as R
import Data.Array.Repa.Repr.Vector
import Data.Maybe

import Vector
import Ray
import Colour
import Shape

data SceneConfig = SceneConfig { width :: Int
                               , height :: Int
                               , lowerLeft :: Vector
                               , horizontal :: Vector
                               , vertical :: Vector
                               , origin :: Vector
                               , world :: ShapeList
                               } deriving (Show)  

render :: SceneConfig -> IO (DynamicImage)
render conf = do
    renderedScene <- R.computeP $ renderScene conf
    return $ ImageRGB8 $ toImage renderedScene

renderScene :: SceneConfig -> Array D DIM2 Colour
renderScene conf = R.fromFunction (Z :. width conf :. height conf) (renderPixel conf)

renderPixel :: SceneConfig -> DIM2 -> Colour
renderPixel conf (Z :. i :. j) = getColour (Ray (origin conf) direction) (world conf)
    where direction = lowerLeft conf + scale (horizontal conf) u + scale (vertical conf) v
          u = fromIntegral i / fromIntegral (width conf)
          v = fromIntegral j / fromIntegral (height conf)

toImage :: Array V DIM2 Colour -> Image PixelRGB8
toImage a = generateImage gen width height
    where Z :. width :. height = R.extent a
          gen x y = pixel $ a ! (Z :. x :. (height - y- 1))
          {-# INLINE gen #-}

getColour :: Ray -> ShapeList -> Colour
getColour ray world = if isJust hitRecord
    then scaleColour (Colour (x n + 1) (y n + 1) (z n + 1)) 0.5
    else scaleColour (Colour 1 1 1) (1 - tMiss) + scaleColour (Colour 0.5 0.7 1) tMiss
    where hitRecord = hit ray (0, maxFloat (0 :: Double)) world 
          tMiss = 0.5 * (y unitDir + 1)
          unitDir = unitVector (dir ray)
          n = normal $ fromJust hitRecord
          maxFloat a = encodeFloat m n where
              b = floatRadix a
              e = floatDigits a
              (_, e') = floatRange a
              m = b ^ e - 1
              n = e' - e