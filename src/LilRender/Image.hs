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

import Data.Array.Repa         as R
import qualified Data.Vector.Unboxed     as V

import LilRender.Color

type Image a = Array U DIM2 a
type ZBufferedImage a = Array U DIM2 (a, Int)

type ImageIndexType = DIM2
type ZBufferedImageIndexType = DIM2

makeImage :: Int -> Int -> RGBColor -> Image RGBColor
makeImage width height color = fromUnboxed (Z .: width .: height) $ V.replicate (width * height) color

twoD :: ZBufferedImage a -> Image a
twoD = R.map fst
