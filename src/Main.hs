module Main where

import qualified Image.NamedColors        as NC

import           Control.Monad.Random
import           Image
import           Image.Color
import           Image.Drawing.Primitives
import           Image.Mutable
import           Math.Vector
import           Model.Wavefront
import           Renderer

import Debug.Trace

main :: IO ()
main = do
    image <- thawImage $ makeImage 1600 1600 NC.black
    model <- loadWavefrontObj "african_head.obj"

    let lightingDirection = Vector3 0 0 (-1) :: Vector3 Double

    drawFlatShadedModel image model (\normal -> return $ (\a -> traceShow a a) $ RGBColor 255 255 255 (max 0 $ round (255 * dotVect normal lightingDirection)))

    image' <- freezeImage image

    writeImage TGA "head.tga" image'

    putStrLn "Wakka wakka!"
