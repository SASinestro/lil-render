module LilRender.Shader (Shader, vertexShader, fragmentShader) where

import Control.Monad.Primitive
import LilRender.Color
import LilRender.Math.Geometry
import LilRender.Model
import Linear
import Linear.Affine

class Shader s where
    vertexShader   :: (PrimMonad m) => s (PrimState m) -> Triangle Vertex -> m (Triangle (Point V3 Double))
    fragmentShader :: (PrimMonad m) => s (PrimState m) -> m (Point V3 Double -> RGBColor)
