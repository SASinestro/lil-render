module LilRender.Shader.Library (
      GouraudShader, gouraudShader
    , PhongShader, phongShader
    ) where

import           LilRender.Color
import qualified LilRender.Color.Named       as NC
import           LilRender.Math.Geometry
import           LilRender.Math.Transform
import           LilRender.Math.Vector
import           LilRender.Model
import           LilRender.Shader
import           LilRender.Texture

import           Control.Monad.Primitive
import qualified Data.Vector.Mutable as MV

data GouraudShader st = GouraudShader {
      _gouraudStateVector           :: MV.MVector st Double
    , _gouraudLightingDirection     :: World (Vector3 Double)
    , _gouraudModelToWorldTransform :: Transform (ModelSpace (Vector3 Double)) (World (Vector3 Double))
}

instance Shader GouraudShader where
    vertexShader (GouraudShader state (World lightDirection) modelToWorldTransform) vertex@(Vertex _ _ (Just (VertexNormal modelNormal))) nthVertex = do
        let (World worldNormal) = transform modelToWorldTransform modelNormal
        MV.write state nthVertex (dotVect worldNormal lightDirection)
        return vertex

    fragmentShader GouraudShader { _gouraudStateVector = state } _ = do
        n1 <- MV.read state 0
        n2 <- MV.read state 1
        n3 <- MV.read state 2
        return (\point -> Just $ NC.orange `scaleColor` triangularInterpolate n1 n2 n3 point)

gouraudShader :: (PrimMonad m) => World (Vector3 Double) -> Transform (ModelSpace (Vector3 Double)) (World (Vector3 Double)) -> m (GouraudShader (PrimState m))
gouraudShader dir modelToWorld = do
    state <- MV.new 3
    return $ GouraudShader state dir modelToWorld

data PhongShader st = PhongShader {
      _phongStateVector           :: MV.MVector st (Double, Point2 Double)
    , _phongLightingDirection     :: World (Vector3 Double)
    , _phongModelToWorldTransform :: Transform (ModelSpace (Vector3 Double)) (World (Vector3 Double))
}

instance Shader PhongShader where
    vertexShader (PhongShader state (World lightDirection) modelToWorldTransform) vertex@(Vertex _ (Just (TextureCoordinate textureCoord)) (Just (VertexNormal modelNormal))) nthVertex = do
        let (World worldNormal) = transform modelToWorldTransform modelNormal
        MV.write state nthVertex (dotVect worldNormal lightDirection, textureCoord)
        return vertex

    fragmentShader PhongShader { _phongStateVector = state } texture = do
        (n1, Point2 t1x t1y) <- MV.read state 0
        (n2, Point2 t2x t2y) <- MV.read state 1
        (n3, Point2 t3x t3y) <- MV.read state 2

        return (\point -> do
            let tx = triangularInterpolate t1x t2x t3x point
            let ty = triangularInterpolate t1y t2y t3y point
            let color = getColorFromTexture texture (TextureCoordinate (Point2 tx ty))

            Just (scaleColor color $ triangularInterpolate n1 n2 n3 point)
            )

phongShader :: (PrimMonad m) => World (Vector3 Double) -> Transform (ModelSpace (Vector3 Double)) (World (Vector3 Double)) -> m (PhongShader (PrimState m))
phongShader dir modelToWorld = do
    state <- MV.new 3
    return $ PhongShader state dir modelToWorld
