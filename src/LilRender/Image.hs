module LilRender.Image (
      Image(..)
    , ImageIndexType
    , (<!>)
    , ImageFormat(..)
    , loadImage
    , saveImage
    , drawImageWith
    ) where

import Control.Monad              (liftM)

import LilRender.Image.Format.TGA (readTGA, writeTGA)
import LilRender.Image.Immutable
import LilRender.Image.Mutable

data ImageFormat = TGA

loadImage :: ImageFormat -> FilePath -> IO Image
loadImage TGA = liftM toImage . readTGA

saveImage :: ImageFormat -> FilePath -> Image -> IO ()
saveImage TGA path img = writeTGA path $ fromImage img
