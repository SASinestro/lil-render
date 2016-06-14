module Main where

import Image
import Image.Mutable

import Image.Color
import qualified Image.NamedColors as NC

import Image.Drawing.Primitives

import Control.Monad


main :: IO ()
main = do
    image <- thawImage $ makeImage 100 100 NC.black

    drawLine image NC.white (13, 20) (80, 40)

    image' <- freezeImage image

    writeImage TGA "image.tga" image'