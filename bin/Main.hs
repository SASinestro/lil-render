module Main where

import           LilRender.Color
import qualified LilRender.Color.Named    as NC
import           LilRender.Image
import           LilRender.Math.Geometry
import           LilRender.Model
import           LilRender.Renderer
import           LilRender.Shader.Library
import           LilRender.Texture
import Linear
import Linear.Affine
import Control.Monad
import           Criterion.Measurement    (getTime, initializeTime, secs)

width = 800
height = 800

lightDirection = V3 1 1 1

--

eye       = V3 1.0 1.0 3.0
center    = V3 0.0 0.0 0.0
up        = V3 0.0 1.0 0.0
cameraMat = mInv !*! translate
    where
        z@(V3 z1 z2 z3)  = signorm $ eye ^-^     center -- z is a normal vector from the camera's location to its target
        x@(V3 x1 x2 x3)  = signorm $ up  `cross` z
        (V3 y1 y2 y3)    = signorm $ z   `cross` x
        (V3 ctx cty ctz) = if center == zero then zero else signorm center
        mInv = V4
            (V4 x1 y1 z1 0)
            (V4 x2 y2 z2 0)
            (V4 x3 y3 z3 0)
            (V4  0  0  0 1)
        translate = V4
            (V4 1 0 0 (-ctx))
            (V4 0 1 0 (-cty))
            (V4 0 0 1 (-ctz))
            (V4 0 0 0      1)

perspectiveMat = 
    V4
        (V4 1 0        0 0)
        (V4 0 1        0 0)
        (V4 0 0        1 0)
        (V4 0 0 (-scale) 1)
    where
        scale = recip $ norm (eye ^-^ center)

viewportMat = V4
    (V4 (w / 2)     0            0       0)
    (V4 0           (h / 2)      0       0)
    (V4 0           0            (d / 2) 0)
    (V4 (x + w / 2) (y + h / 2)  (d / 2) 1)
    where
        w = 600
        h = 600
        x = 100
        y = 100
        d = 255

modelToScreenMat = cameraMat !*! perspectiveMat !*! viewportMat 

--

rgbToVector :: RGBColor -> V3 Double
rgbToVector (RGBColor r g b) = V3 (r' - 1) (g' - 1) (b' - 1)
    where
        r' = (fromIntegral r) / 127
        g' = (fromIntegral g) / 127
        b' = (fromIntegral b) / 127

main :: IO ()
main = 
    -- print `mapM_` [viewportMat, perspectiveMat, cameraMat, modelToScreenMat]
    do
        !model <- loadWavefrontModel "data/african_head/african_head.obj"
        Right !diffuse  <- loadImage RGB "data/african_head/african_head_diffuse.tga"
        Right !normalTex <- loadImage RGB "data/african_head/african_head_nm.tga"

        let normal = imgmap rgbToVector normalTex

        !shader <- phongShader (signorm lightDirection) identity diffuse normal

        initializeTime

        replicateM_ 5 $ do
            startTime <- getTime
            !image <- drawImageWith width height NC.black $ drawTexturedModel model shader modelToScreenMat
            endTime <- getTime

            putStrLn $ "Frame time: " ++ secs (endTime - startTime)

            writeTGA "head.tga" image
