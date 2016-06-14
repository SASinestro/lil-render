module Main where

import qualified Image.NamedColors  as NC

import           Image
import           Model
import           Model.Wavefront
import           Renderer.Wireframe

main :: IO ()
main = do
    model <- loadWavefrontObj "african_head.obj"

    image <- wireframeImage model 800 800 NC.black NC.white

    writeImage TGA "head.tga" image

    putStrLn "Wakka wakka!"
