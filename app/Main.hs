module Main where

import Impede
import Ray
import Codec.Picture
import Control.Monad
import System.Environment (getArgs)

main :: IO ()
main = do
  let sceneConfig = SceneConfig { width = 200
                                , height = 100 
                                , lower_left = Vector(-2, -1, -1)
                                , horizontal = Vector(4, 0, 0)
                                , vertical = Vector(0, 2, 0)
                                , origin = Vector(0, 0, 0)
                                }
  [path] <- getArgs
  savePngImage path $ render sceneConfig