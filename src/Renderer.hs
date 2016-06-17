module Renderer where

import Control.Lens
import Control.Monad
import Control.Monad.Primitive
import Data.Maybe

import Image
import Image.Color
import Image.DrawingPrimitives
import Image.Mutable
import Image.Texture

import Model

import Math.Geometry
import Math.Matrix
import Math.Matrix.Transform
import Math.Vector

pointToTextureCoords :: TextureCoordinate -> TextureCoordinate -> TextureCoordinate -> Barycentric (Point3 Double) -> TextureCoordinate
pointToTextureCoords (TextureCoordinate (Point2 t1x t1y)) (TextureCoordinate (Point2 t2x t2y)) (TextureCoordinate (Point2 t3x t3y)) bary = TextureCoordinate (Point2 x y)
    where
        x = triangularInterpolate (Triangle t1x t2x t3x) bary
        y = triangularInterpolate (Triangle t2y t2y t3y) bary

drawTexturedModel :: forall m. (PrimMonad m) => MutableImage (PrimState m) -- Target
                                             -> Model -- The model to be rendered
                                             -> Transform -- Matrix to transform world coordinates into screen coordinates
                                             -> Texture -- The texture
                                             -> (Face -> Barycentric (Point3 Double) -> RGBColor -> m RGBColor) -- For lighting, etc
                                             -> m ()
drawTexturedModel image model projection texture colorMutator =
    mapM_ (\face -> drawFilledTriangle image (getTextureColor face) (screenTriangleForFace face)) $ faces model
    where
        screenTriangleForFace :: Face -> Triangle (Screen (Point3 Double))
        screenTriangleForFace face = Triangle p1 p2 p3
            where
                (Face
                    (Vertex (VertexPoint (World p1')) _ _)
                    (Vertex (VertexPoint (World p2')) _ _)
                    (Vertex (VertexPoint (World p3')) _ _)) = face
                p1 = Screen $ transform3DPoint projection p1'
                p2 = Screen $ transform3DPoint projection p2'
                p3 = Screen $ transform3DPoint projection p3'

        getTextureColor :: Face -> Barycentric (Point3 Double) -> m RGBColor
        getTextureColor face barycentric = colorMutator face barycentric . getColorFromTexture texture $ pointToTextureCoords t1 t2 t3 barycentric
            where
                (Face (Vertex _ (Just t1) _)
                      (Vertex _ (Just t2) _)
                      (Vertex _ (Just t3) _)) = face
