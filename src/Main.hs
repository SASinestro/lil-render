module Main where

import           Control.DeepSeq         (force)
import           Control.Exception       (evaluate)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Primitive
import           Criterion.Measurement   (getTime, initializeTime, secs)
import           Data.Foldable

import           Image
import           Image.Color
import           Image.Mutable
import qualified Image.NamedColors       as NC
import           Image.Texture
import           Math.Geometry
import           Math.Matrix
import           Math.Matrix.Transform
import           Math.Vector
import           Model
import           Model.Wavefront
import           Renderer

lightIntensityAtPointOnFace :: Vector3 Double -> Face -> Barycentric (Point3 Double) -> Double
lightIntensityAtPointOnFace lightDirection face = triangularInterpolate (Triangle i1 i2 i3)
    where
        (Face (Vertex _ _ (Just (VertexNormal (World n1))))
              (Vertex _ _ (Just (VertexNormal (World n2))))
              (Vertex _ _ (Just (VertexNormal (World n3))))) = face
        i1 = n1 `dotVect` lightDirection
        i2 = n2 `dotVect` lightDirection
        i3 = n3 `dotVect` lightDirection


lookAt eye center up =
    identityMatrix 4 `mUpdate` concat [ [ ((1, i), x' !! (i - 1)),
                                          ((2, i), y' !! (i - 1)),
                                          ((3, i), z' !! (i - 1)),
                                          ((i, 4), 0            )  ] | i <- [1 .. 3] ]
    where
        z  = normalizeVect $ eye - center
        z' = toList z
        x  = normalizeVect (up `crossVect` z)
        x' = toList x
        y  = normalizeVect (z `crossVect` x)
        y' = toList y

eye = Vector3 1 1 3
center = Vector3 0 0 0
up = Vector3 0 1 0

viewPort w h scale = viewportTransform (Point2 (round $ width / 8) (round $ height / 8)) (round $ scale * width) (round $ scale * height) 255
    where
        width = fromIntegral w :: Double
        height = fromIntegral h :: Double
projection = identityMatrix 4 `mUpdate` [((4, 3), -1 / magnitudeVect eye)]
look = lookAt eye center up
toScreenCoords w h = viewPort w h 0.75 `mMult` projection `mMult` look

lightingDirection = normalizeVect $ Vector3 1 (-1) 1
lightAt = lightIntensityAtPointOnFace lightingDirection

drawModel :: Int -> Int -> Model -> Texture -> IO Image
drawModel width height !model !texture = do
    image <- thawImage $ makeImage width height NC.red
    drawTexturedModel image model (toScreenCoords width height) texture (\face point color -> return . scaleColor NC.white $ lightAt face point)
    freezeImage image

main :: IO ()
main = do
    model <- loadWavefrontObj "data/african_head/african_head.obj"
    texture <- readImage TGA "data/african_head/african_head_diffuse.tga"

    initializeTime

    startTime <- getTime
    img <- drawModel 400 400 model (Texture texture)
    endTime <- getTime

    putStrLn $ "Frame time: " ++ secs (endTime - startTime)

    writeImage TGA "head.tga" img