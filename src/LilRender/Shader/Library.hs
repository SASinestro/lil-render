module LilRender.Shader.Library (
      GouraudShader, gouraudShader
    , PhongShader, phongShader
    ) where

import           LilRender.Color
import qualified LilRender.Color.Named    as NC
import           LilRender.Math.Geometry
import           Linear
import           Linear.Affine
import LilRender.Image
import           LilRender.Model
import           LilRender.Shader
import           LilRender.Texture
import Data.Bifunctor
import           Control.Monad.Primitive
import Data.Maybe
import Data.Primitive.MutVar

--

data GouraudShader st = GouraudShader {
      _gouraudState    :: MutVar st (Triangle Double)
    , _gouraudLightDir :: V3 Double
    , _gouraudWorldMat :: M44 Double
}

instance Shader GouraudShader where
    {-# INLINE vertexShader #-}
    vertexShader GouraudShader{..} triangle = do
        writeMutVar _gouraudState $ dot _gouraudLightDir . transformVector _gouraudWorldMat . fromJust . _vertexNormal <$> triangle
        return $ _point <$> triangle

    {-# INLINE fragmentShader #-}
    fragmentShader GouraudShader{ _gouraudState = state } = do
        normals <- readMutVar state
        return (\point -> NC.white `scaleColor` triangularInterpolate normals point)

gouraudShader :: (PrimMonad m) => V3 Double -> M44 Double -> m (GouraudShader (PrimState m))
gouraudShader dir modelToWorld = do
    state <- newMutVar (Triangle 0 0 0)
    return $ GouraudShader state dir modelToWorld

--

data PhongShader st = PhongShader {
      _phongState    :: MutVar st (Triangle (Point V2 Double))
    , _phongLightDir :: V3 Double
    , _phongWorldMat :: M44 Double
    , _phongDiffuse  :: Image RGBColor
    , _phongNormal   :: Image (V3 Double)
}

instance Shader PhongShader where
    {-# INLINE vertexShader #-}
    vertexShader PhongShader{..} triangle = do
        writeMutVar _phongState $ fromJust . _textureCoordinate <$> triangle
        return $ _point <$> triangle

    {-# INLINE fragmentShader #-}
    fragmentShader PhongShader {..} = do
        textureTri <- readMutVar _phongState

        return (\point -> do
            let texture   = triangularInterpolateVector textureTri point

            let diffuse   = getColorFromTexture _phongDiffuse texture
            let normal    = getColorFromTexture _phongNormal texture
            let intensity = dot normal _phongLightDir

            diffuse `scaleColor` intensity
            )

phongShader :: (PrimMonad m) => V3 Double -> M44 Double -> Image RGBColor -> Image (V3 Double) -> m (PhongShader (PrimState m))
phongShader dir modelToWorld diffuse normal = do
    state <- newMutVar (Triangle zero zero zero)
    return $ PhongShader state dir modelToWorld diffuse normal
