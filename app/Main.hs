module Main where

import Lib
import Codec.Picture
import Control.Monad
import Control.Monad.ST
import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))
import System.Environment (getArgs)
import qualified Codec.Picture.Types as M
import qualified Data.Array.Repa     as R -- for Repa

main :: IO ()
main = do
  [path] <- getArgs
  savePngImage path generateImg

generateImg :: DynamicImage
generateImg = ImageRGB8 (generateImage originalFnc 1200 1200)

originalFnc :: Int -> Int -> PixelRGB8
originalFnc x y = PixelRGB8 0 0 0