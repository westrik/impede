module Main where

import Impede
import Codec.Picture
import System.Environment (getArgs)

main :: IO ()
main = do
  let sceneConfig = SceneConfig { width = 400
                                , height = 200
                                , camera = Camera (Vector (-2) (-1) (-1))
                                                  (Vector 4 0 0) 
                                                  (Vector 0 2 0) 
                                                  (Vector 0 0 0)
                                , world = ShapeList [
                                            SphereShape (Sphere (Vector 0 0 (-1)) 0.5), 
                                            SphereShape (Sphere (Vector 0 (-100.5) (-1)) 100)
                                          ]
                                , iterations = 1
                                }
  [path] <- getArgs
  image <- render sceneConfig
  savePngImage path image