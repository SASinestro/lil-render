module Renderer.Wireframe where

import Control.Monad
import Control.Monad.Primitive

import Image
import Image.Mutable
import Image.Color
import Image.Drawing.Primitives
import Math.Vector
import Model

wireframeImage :: (PrimMonad m) => Model -> Int -> Int -> RGBColor -> RGBColor -> m Image
wireframeImage model w h bg fg = do
    image <- thawImage $ makeImage w h bg

    let viewportWidth = fromIntegral (w - 1) :: Double
    let viewportHeight = fromIntegral (h - 1) :: Double

    let drawLine' = drawLine image fg

    let verticesForFace face = _vertex <$> face
    let vectorsForFace face = [ (t !! i,  t !! ( (i + 1) `mod` 3 ) ) | i <- [0..2], let t = verticesForFace face ]
    let vectorToPoint vect = ( round (( 1 + _v3_x vect ) * viewportWidth / 2) , round (( 1 + _v3_y vect ) * viewportHeight / 2) )

    sequence_ $ mapM_ (\(a, b) -> drawLine' (vectorToPoint a) (vectorToPoint b)) <$> (vectorsForFace <$> _faces model)

    freezeImage image
