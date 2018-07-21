module Image where

import qualified Data.Vector.Storable as V

import Colour

data Image = Image { imageWidth :: Int
                   , imageHeight :: Int
                   , imageData :: V.Vector Colour
                   }