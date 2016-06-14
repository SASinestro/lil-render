module Renderer where

import Control.Lens
import Control.Monad
import Control.Monad.Primitive

import Image
import Image.Color
import Image.Drawing.Primitives
import Image.Mutable
import Math.Vector
import Model

drawWireframeModel :: (PrimMonad m) => MutableImage (PrimState m) -> Model -> RGBColor -> m ()
drawWireframeModel image model color = do
    let viewportWidth = fromIntegral (_mWidth image - 1) :: Double
    let viewportHeight = fromIntegral (_mHeight image - 1) :: Double

    let drawLine' = drawLine image color

    let vectorsForFace face = [ (t !! i,  t !! ( (i + 1) `mod` 3 ) ) | i <- [0..2], let t = _vertex <$> face ]
    let vectorToPoint vect = ( round (( 1 + _v3_x vect ) * viewportWidth / 2) , round (( 1 + _v3_y vect ) * viewportHeight / 2) )

    sequence_ $ mapM_ (\(a, b) -> drawLine' (vectorToPoint a) (vectorToPoint b)) <$> (vectorsForFace <$> _faces model)

drawFlatShadedModel :: (PrimMonad m) => MutableImage (PrimState m) -> Model -> (Vector3 Double -> m RGBColor) -> m ()
drawFlatShadedModel image model getColor = do
    let viewportWidth = fromIntegral (_mWidth image - 1) :: Double
    let viewportHeight = fromIntegral (_mHeight image - 1) :: Double
    let vectorToScreen (Vector3 x y _) = (round ((x + 1) * viewportWidth/2), round ((y + 1) * viewportHeight/2))

    let vectorsByFace = (_vertex <$>) <$> _faces model

    let v1 = (!! 0)
    let v2 = (!! 1)
    let v3 = (!! 2)

    let normal vs = normalizeVect $ crossVect (v3 vs `subVect` v1 vs) (v2 vs `subVect` v1 vs)

    mapM_ (\vectors -> do
        color <- getColor . normal $ vectors
        drawFilledTriangle image color (vectorToScreen . v1 $ vectors, vectorToScreen . v2 $ vectors, vectorToScreen . v3 $ vectors)) vectorsByFace

