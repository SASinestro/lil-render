module Main where

import qualified LilRender.Color.Named    as NC
import           LilRender.Image
import           LilRender.Math.Geometry
import           LilRender.Math.Transform
import           LilRender.Math.Vector
import           LilRender.Model
import           LilRender.Renderer
import           LilRender.Shader.Library
import           LilRender.Texture

import           Criterion.Measurement    (getTime, initializeTime, secs)

width = 800
height = 800

lightDirection = World (Vector3 1.0 1.0 1.0)

center :: Int -> Int -> Screen (Point2 Int)
center width height = Screen (Point2 x y)
    where
        x = round ((fromIntegral width) / 8)
        y = round ((fromIntegral height) / 8)

scale' :: Double -> (Int -> Int)
scale' factor a = round ((fromIntegral a) * factor)

main :: IO ()
main = do
    model <- loadModel WavefrontOBJ "data/african_head/african_head.obj"
    texture <- loadTexture TGA "data/african_head/african_head_diffuse.tga"

    let cameraLocation = World (Point3 1.0 1.0 3.0)
    let cameraTarget   = World (Point3 0.0 0.0 0.0)
    let camera = cameraTransform cameraLocation cameraTarget

    let scale = scale' (3/4)
    let viewport = viewportTransform (center width height) (scale width) (scale height)

    let modelToScreen = (identityTransform :: Transform (ModelSpace (Point3 Double)) (World (Point3 Double))) >>> camera >>> (orthographicProjectionTransform cameraLocation) >>> viewport

    shader <- phongShader (normalizeVect <$> lightDirection) identityTransform

    initializeTime

    startTime <- getTime
    image <- drawImageWith width height NC.royalBlue (\image -> drawTexturedModel image model texture shader modelToScreen)
    endTime <- getTime

    putStrLn $ "Frame time: " ++ secs (endTime - startTime)

    saveImage TGA "head.tga" image
