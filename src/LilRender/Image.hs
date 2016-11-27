module LilRender.Image (
      Image(..)
    , ImageIndexType
    , (<!>)
    , makeImage
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
