module Main where

import qualified Image.NamedColors        as NC

import           Image
import           Image.Drawing.Primitives
import           Image.Mutable
import           Math.Vector

main :: IO ()
main = do
    image <- thawImage $ makeImage 200 200 NC.black

    let t1 = (( 10,  70), ( 50, 160), (70,   80))
    let t2 = ((180,  50), (150,   1), (70,  180))
    let t3 = ((180, 150), (120, 160), (130, 180))

    drawFilledTriangle image NC.red t1
    drawFilledTriangle image NC.white t2
    drawFilledTriangle image NC.green t3

    image' <- freezeImage image

    writeImage TGA "triangles.tga" image'

    putStrLn "Wakka wakka!"
