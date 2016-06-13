module Main where

import Image
import Image.Mutable

import Image.Color
import qualified Image.NamedColors as NC

import Image.Drawing.Primitives

import Control.Monad


main :: IO ()
main = do
    image <- thawImage $ make_image 100 100 NC.black

    drawLine image (13, 20) (80, 40) NC.white

    image' <- freezeImage image

    write_image TGA "image.tga" image'