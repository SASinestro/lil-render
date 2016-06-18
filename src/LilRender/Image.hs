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

import Control.Monad              (liftM)

import LilRender.Image.Format.TGA (readTGAIO, writeTGA)
import LilRender.Image.Immutable
import LilRender.Image.Mutable

data ImageFormat = TGA

loadImage :: ImageFormat -> FilePath -> IO Image
loadImage TGA = liftM toImage . readTGAIO

saveImage :: ImageFormat -> FilePath -> Image -> IO ()
saveImage TGA path img = writeTGA path $ fromImage img
