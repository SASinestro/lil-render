module LilRender.Texture (getColorFromTexture) where

import qualified Data.Vector.Storable    as V
import Foreign.Storable
import LilRender.Image
import Linear
import Linear.Affine

{-# INLINE getColorFromTexture #-}
getColorFromTexture :: (Storable a) => Image a -> Point V2 Double -> a
getColorFromTexture image@Image{..} (P (V2 a b)) = image <!> (P $ V2 (round $ a * width) (round $ b * height))
    where
        width = fromIntegral _width
        height = fromIntegral _height

