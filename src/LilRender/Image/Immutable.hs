module LilRender.Image.Immutable (
      Image(..)
    , ImageIndexType
    , (<!>)
    , makeImage
) where

import           Data.STBImage
import qualified Data.Vector.Storable    as V
import           LilRender.Math.Geometry

type ImageIndexType = Point2 Int

{-# INLINE (<!>) #-}
(<!>) :: (V.Storable a, Color a) => Image a -> ImageIndexType -> a
Image{..} <!> (Point2 x y) = _pixels `V.unsafeIndex` (_width * (_width - (y + 1)) + x)

makeImage :: (V.Storable a, Color a) => Int -> Int -> a -> Image a
makeImage _width _height color = let _pixels = V.replicate (_width * _height) color in Image{..}
