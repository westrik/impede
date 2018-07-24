{-# LANGUAGE TypeOperators #-}

module Impede
    ( Vector (..)
    , Camera (..)
    , SceneConfig (..)
    , Shape (..)
    , ShapeList (..)
    , Sphere (..)
    
    , render
    ) where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import qualified Codec.Picture.Types as M
import Data.Maybe
import qualified Data.Vector as V
import System.Random


import Control.DeepSeq (NFData(..))
import Control.Parallel.Strategies
import qualified Data.Vector.Generic as VG

import Camera
import Vector
import Ray
import Colour
import Shape

data SceneConfig = SceneConfig { width :: Int
                               , height :: Int
                               , camera :: Camera
                               , world :: ShapeList
                               , iterations :: Int
                               } deriving (Show)  

render :: SceneConfig -> IO (DynamicImage)
render conf = do
    renderedScene <- renderScene conf 
    return $ ImageRGB8 $ toImage renderedScene conf

renderScene :: SceneConfig -> IO [Colour]
renderScene conf = sequence ((map (renderPixel conf) [0 .. width conf * height conf - 1])
                                `using` parList rdeepseq)

renderPixel :: SceneConfig -> Int -> IO (Colour)
renderPixel conf i = do
    gen <- newStdGen
    return $ averageColours (map renderIter $ randomPairs gen)
    where renderIter (s, t) = getColour (getRay (camera conf) (u s) (v t)) (world conf)
          u r = (fromIntegral (i `mod` (width conf)) + r) / fromIntegral (width conf)
          v r = (fromIntegral (i `div` (height conf)) + r) / fromIntegral (height conf)
          randomPairs g = zip (take (iterations conf) $ randoms g)
                              (take (iterations conf) $ randoms g)

toImage :: [Colour] -> SceneConfig -> Image PixelRGB8
toImage a conf = generateImage gen w h
    where gen x y = pixel $ (V.fromList a) V.! (y * h + x)
          w = width conf
          h = height conf
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