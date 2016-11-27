module LilRender.Shader (Shader, vertexShader, fragmentShader) where

import LilRender.Color
import LilRender.Math.Geometry
import LilRender.Model
import LilRender.Texture

import Control.Monad.Primitive

type PointOnFace = Barycentric (Point3 Double)

class Shader s where
    vertexShader :: (PrimMonad m) => s (PrimState m) -> Vertex -> Int -> m Vertex
    fragmentShader :: (PrimMonad m) => s (PrimState m) -> Texture -> m (PointOnFace -> RGBColor)
