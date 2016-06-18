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
import           Data.Primitive.MutVar
import qualified Data.Vector.Unboxed.Mutable as MV

data GouraudShader st = GouraudShader {
      _stateVector       :: MV.MVector st Double
    , _stateVectorIdx    :: MutVar st Int
    , _lightingDirection :: World (Vector3 Double)
    , _modelToWorldTransform :: Transform (ModelSpace (Vector3 Double)) (World (Vector3 Double))
}

instance Shader GouraudShader where
    vertexShader GouraudShader {   _stateVector=state
                                  , _stateVectorIdx=idx
                                  , _lightingDirection=(World lightDirection)
                                  , _modelToWorldTransform=modelToWorldTransform
                                } vertex@(Vertex _ _ (Just (VertexNormal modelNormal))) = do
        index' <- readMutVar idx
        let index = if index' < 3 then index' else 0
        let (World worldNormal) = transform modelToWorldTransform modelNormal
        MV.write state index (dotVect worldNormal lightDirection)
        modifyMutVar idx (+ 1)
        return vertex

    fragmentShader GouraudShader { _stateVector = state } _ = do
        n1 <- MV.read state 0
        n2 <- MV.read state 1
        n3 <- MV.read state 2
        return (\point -> Just $ NC.white `scaleColor` triangularInterpolate n1 n2 n3 point)

gouraudShader :: (PrimMonad m) => World (Vector3 Double) -> Transform (ModelSpace (Vector3 Double)) (World (Vector3 Double)) -> m (GouraudShader (PrimState m))
gouraudShader dir modelToWorld = do
    state <- MV.new 3
    idx <- newMutVar 0
    return $ GouraudShader state idx dir modelToWorld
