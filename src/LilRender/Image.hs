module LilRender.Image (
      Image(..)
    , ImageIndexType
    , (<!>)
    , ImageFormat(..)
    , makeImage
    , thawImage
    , freezeImage
    , loadImage
    , saveImage
    , drawImageWith
    ) where

import LilRender.Image.Immutable
import LilRender.Image.Mutable
import LilRender.Image.STB

data ImageFormat = TGA | PNG | BMP

loadImage :: FilePath -> IO Image
loadImage = stbLoadImage

saveImage :: ImageFormat -> FilePath -> Image -> IO ()
saveImage TGA = stbWriteTGA
saveImage PNG = stbWritePNG
saveImage BMP = stbWriteBMP
