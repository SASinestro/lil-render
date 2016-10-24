module LilRender.Texture (Texture, getColorFromTexture, loadTexture, ImageFormat(..)) where

import Control.DeepSeq
import Control.Monad           (liftM)
import LilRender.Color
import LilRender.Image
import LilRender.Math.Geometry
import LilRender.Model

newtype Texture = Texture Image deriving (Show, Eq, NFData)

{-# INLINE getColorFromTexture #-}
getColorFromTexture :: Texture -> TextureCoordinate -> RGBColor
getColorFromTexture (Texture image@(Image _ width' height')) (TextureCoordinate (Point2 a b)) = image <!> Screen (Point2 (round $ a * width) (round $ b * height))
    where
        width = fromIntegral width'
        height = fromIntegral height'

loadTexture :: ImageFormat -> FilePath -> IO Texture
loadTexture format path = liftM Texture (loadImage format path)
