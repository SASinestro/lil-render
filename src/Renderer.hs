module Renderer where

import Control.Lens
import Control.Monad
import Control.Monad.Primitive
import Data.Maybe

import Image
import Image.Color
import Image.Drawing.Primitives
import Image.Mutable
import Math.Matrix
import Math.Matrix.Transform
import Math.Vector
import Model

drawTexturedModel :: forall m. (PrimMonad m) => MutableImage (PrimState m) -- Target
                                             -> Model -- The model to be rendered
                                             -> Transform -- Matrix to transform world coordinates into screen coordinates
                                             -> Image -- The texture
                                             -> (Face -> Vector3 Double -> RGBColor -> m RGBColor) -- For lighting, etc
                                             -> m ()
drawTexturedModel image model projection texture colorMutator =
    mapM_ (\face -> drawFilledTriangle image (getTextureColor face) (screenTriangleForFace face)) $ model ^. faces
    where
        textureWidth = fromIntegral $ texture ^. width
        textureHeight = fromIntegral $ texture ^. height

        textureCoordinateForPoint :: TextureCoordinate -> TextureCoordinate -> TextureCoordinate -> Vector3 Double -> ImageIndexType
        textureCoordinateForPoint (Vector2 a1 a2) (Vector2 b1 b2) (Vector2 c1 c2) (Vector3 ta tb tc) =
            (round ((a1 * ta + b1 * tb + c1 * tc) * textureWidth), round ((a2 * ta + b2 * tb + c2 * tc) * textureHeight))

        screenTriangleForFace :: Face -> Triangle
        screenTriangleForFace face = (v1, v2, v3)
            where (v1:v2:v3:_) = (fmap . fmap) round $ transform3DVector projection <$> face ^.. vertices . point

        getTextureColor :: Face -> Vector3 Double -> m RGBColor
        getTextureColor face barycentric = colorMutator face barycentric $ texture <!> textureCoordinateForPoint t1 t2 t3 barycentric
            where
                (t1:t2:t3:_) = face ^.. vertices . textureCoordinate . _Just
