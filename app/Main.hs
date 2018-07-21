module Main where

import Impede
import Codec.Picture
import System.Environment (getArgs)

main :: IO ()
main = do
  let sceneConfig = SceneConfig { width = 4096
                                , height = 2048
                                , lowerLeft = Vector (-2) (-1) (-1)
                                , horizontal = Vector 4 0 0
                                , vertical = Vector 0 2 0
                                , origin = Vector 0 0 0
                                }
  [path] <- getArgs
  image <- render sceneConfig
  savePngImage path image