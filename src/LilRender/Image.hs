module LilRender.Image (
      Image(..)
    , ImageIndexType
    , (<!>)
    , makeImage
    , imgmap
    , loadImage
    , ColorFlag(..)
    , drawImageWith
    , writeBMP
    , writePNG
    , writeTGA
    ) where

import Data.STBImage
import LilRender.Image.Immutable
import LilRender.Image.Mutable
