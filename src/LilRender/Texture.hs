module LilRender.Texture (Texture, getColorFromTexture, loadTexture) where

import LilRender.Color
import LilRender.Image
import LilRender.Math.Geometry
import LilRender.Model

newtype Texture = Texture (Image RGBColor) deriving (Show, Eq)

{-# INLINE getColorFromTexture #-}
getColorFromTexture :: Texture -> TextureCoordinate -> RGBColor
getColorFromTexture (Texture image@Image{..}) (TextureCoordinate (Point2 a b)) = image <!> Screen (Point2 (round $ a * width) (round $ b * height))
    where
        width = fromIntegral _width
        height = fromIntegral _height

loadTexture :: FilePath -> IO Texture
loadTexture = fmap Texture . (either fail return =<<) . loadImage RGB
