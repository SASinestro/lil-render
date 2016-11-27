module Bench.Macro where

import           Criterion

import qualified Data.ByteString            as BS
import qualified Data.Text.IO               as T

import qualified LilRender.Color.Named      as NC
import           LilRender.Image
import           LilRender.Image.Format.TGA
import           LilRender.Math.Geometry
import           LilRender.Math.Transform
import           LilRender.Math.Vector
import           LilRender.Model
import           LilRender.Model.Wavefront
import           LilRender.Renderer
import           LilRender.Shader.Library
import           LilRender.Texture

benchLoadModel = env (T.readFile "data/african_head/african_head.obj") $ bench "Load model from .obj" . nf loadWavefrontObj


drawENV width height = do
    model <- loadModel WavefrontOBJ "data/african_head/african_head.obj"
    texture <- loadTexture TGA "data/african_head/african_head_diffuse.tga"

    let cameraLocation = World (Point3 1.0 1.0 3.0)
    let cameraTarget   = World (Point3 0.0 0.0 0.0)
    let camera = cameraTransform cameraLocation cameraTarget

    let scale a = round ((fromIntegral a) * 3/4)
    let viewport = viewportTransform (center width height) (scale width) (scale height)

    let modelToScreen = (identityTransform :: Transform (ModelSpace (Point3 Double)) (World (Point3 Double))) >>> camera >>> (orthographicProjectionTransform cameraLocation) >>> viewport

    return (model, texture, modelToScreen)

lightDirection = World (Vector3 1.0 1.0 1.0)
center :: Int -> Int -> Screen (Point2 Int)
center width height = Screen (Point2 x y)
    where
        x = round ((fromIntegral width) / 8)
        y = round ((fromIntegral height) / 8)

benchDrawImage width height = env (drawENV width height) $ bench "Draw model in memory" . whnfIO . \(model, texture, modelToScreen) -> do
    shader <- phongShader (normalizeVect <$> lightDirection) identityTransform
    drawImageWith width height NC.royalBlue (\image -> drawTexturedModel image model texture shader modelToScreen)

macro width height = bgroup "Macro benchmarks" [
      benchLoadModel
    , benchDrawImage width height
    ]
