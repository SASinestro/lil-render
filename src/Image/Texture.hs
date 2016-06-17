module Image.Texture where

import Image
import Image.Color
import Math.Geometry
import Math.Vector
import Model

newtype Texture = Texture Image deriving (Show, Eq)

getColorFromTexture :: Texture -> TextureCoordinate -> RGBColor
getColorFromTexture (Texture image@(Image _ width' height')) (TextureCoordinate vect) = image <!> (Screen $ scale vect)
    where
        width = fromIntegral width'
        height = fromIntegral height'

        scale :: Point2 Double -> Point2 Double
        scale (Point2 a b) = Point2 (a * width) (b * height)
