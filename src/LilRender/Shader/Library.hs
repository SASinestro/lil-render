module LilRender.Shader.Library (
      GouraudShader, gouraudShader
    ) where

import           LilRender.Color
import qualified LilRender.Color.Named       as NC
import           LilRender.Math.Geometry
import           LilRender.Math.Transform
import           LilRender.Math.Vector
import           LilRender.Model
import           LilRender.Shader

import           Control.Monad.Primitive
import qualified Data.Vector.Unboxed.Mutable as MV

data GouraudShader st = GouraudShader {
      _stateVector       :: MV.MVector st Double
    , _lightingDirection :: World (Vector3 Double)
    , _modelToWorldTransform :: Transform (ModelSpace (Vector3 Double)) (World (Vector3 Double))
}

instance Shader GouraudShader where
    vertexShader (GouraudShader state (World lightDirection) modelToWorldTransform) vertex@(Vertex _ _ (Just (VertexNormal modelNormal))) nthVertex = do
        let (World worldNormal) = transform modelToWorldTransform modelNormal
        MV.write state nthVertex (dotVect worldNormal lightDirection)
        return vertex

    fragmentShader GouraudShader { _stateVector = state } _ = do
        n1 <- MV.read state 0
        n2 <- MV.read state 1
        n3 <- MV.read state 2
        return (\point -> Just $ NC.white `scaleColor` triangularInterpolate n1 n2 n3 point)

gouraudShader :: (PrimMonad m) => World (Vector3 Double) -> Transform (ModelSpace (Vector3 Double)) (World (Vector3 Double)) -> m (GouraudShader (PrimState m))
gouraudShader dir modelToWorld = do
    state <- MV.new 3
    return $ GouraudShader state dir modelToWorld
