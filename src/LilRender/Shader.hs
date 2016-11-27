module LilRender.Shader (Shader, vertexShader, fragmentShader) where

import LilRender.Color
import LilRender.Math.Geometry
import LilRender.Model
import LilRender.Texture

import Control.Monad.Primitive

class Shader s where
    vertexShader :: (PrimMonad m) => s (PrimState m) -> Vertex -> Int -> m Vertex
    fragmentShader :: (PrimMonad m) => s (PrimState m) -> Texture -> m (Point3 Double -> RGBColor)
