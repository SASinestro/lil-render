module LilRender.Image.Immutable (
      Image(..)
    , ImageIndexType
    , (<!>)
    , imgmap
    , makeImage
) where

import           Data.STBImage
import qualified Data.Vector.Storable    as V
import Linear
import Linear.Affine

type ImageIndexType = Point V2 Int

{-# INLINE (<!>) #-}
(<!>) :: (V.Storable a) => Image a -> ImageIndexType -> a
Image{..} <!> (P (V2 x y)) = _pixels `V.unsafeIndex` (_width * (_width - (y + 1)) + x)

imgmap :: (V.Storable a, V.Storable b) => (a -> b) -> Image a -> Image b
imgmap f img@Image{ _pixels = p } = img { _pixels = V.map f p }

makeImage :: (V.Storable a, Color a) => Int -> Int -> a -> Image a
makeImage _width _height color = let _pixels = V.replicate (_width * _height) color in Image{..}
