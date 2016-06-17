module LilRender.Image.Texture (Texture, getColorFromTexture, loadTexture) where

import Control.Monad           (liftM)

import LilRender.Image
import LilRender.Image.Color
import LilRender.Math.Geometry
import LilRender.Math.Vector
import LilRender.Model

newtype Texture = Texture Image deriving (Show, Eq)

getColorFromTexture :: Texture -> TextureCoordinate -> RGBColor
getColorFromTexture (Texture image@(Image _ width' height')) (TextureCoordinate vect) = image <!> (Screen $ scale vect)
    where
        width = fromIntegral width'
        height = fromIntegral height'

        scale :: Point2 Double -> Point2 Double
        scale (Point2 a b) = Point2 (a * width) (b * height)

loadTexture :: FilePath -> IO Texture
loadTexture = liftM Texture . readImage TGA
