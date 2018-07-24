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
    return $ ImageRGB8 $ toImage renderedScene

renderScene :: SceneConfig -> IO (V.Vector Colour)
renderScene conf = V.generateM (width conf) (renderPixel conf) 

renderPixel :: SceneConfig -> Int -> IO (Colour)
renderPixel conf i = do
    g <- newStdGen
    return $ getColour (getRay (camera conf) (u $ head . take 1 $ randoms g) (u $ head . take 1 $ randoms g)) (world conf)
    where u r = (fromIntegral i + r) / fromIntegral (width conf)
          -- v = (fromIntegral j) / fromIntegral (height conf)

toImage :: V.Vector Colour -> Image PixelRGB8
toImage a = generateImage gen (length a) 1
    where gen x _ = pixel $ a V.! x
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