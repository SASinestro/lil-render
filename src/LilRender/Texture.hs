module LilRender.Texture (Texture, getColorFromTexture, loadTexture) where

import LilRender.Color
import LilRender.Image
import LilRender.Math.Geometry

newtype Texture = Texture (Image RGBColor) deriving (Show, Eq)

{-# INLINE getColorFromTexture #-}
getColorFromTexture :: Texture -> Point2 Double -> RGBColor
getColorFromTexture (Texture image@Image{..}) (Point2 a b) = image <!> (Point2 (round $ a * width) (round $ b * height))
    where
        width = fromIntegral _width
        height = fromIntegral _height

loadTexture :: FilePath -> IO Texture
loadTexture = fmap Texture . (either fail return =<<) . loadImage RGB
