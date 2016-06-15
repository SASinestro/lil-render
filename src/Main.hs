module Main where

import           Control.Lens
import           Data.Foldable

import           Image
import           Image.Color
import           Image.Mutable
import qualified Image.NamedColors     as NC
import           Math.Matrix
import           Math.Matrix.Transform
import           Math.Vector
import           Model
import           Model.Wavefront
import           Renderer


lightIntensityAtPointOnFace :: Vector3 Double -> Face -> Vector3 Double -> Double
lightIntensityAtPointOnFace lightDirection face (Vector3 t1 t2 t3) = i1 * t1 + i2 * t2 + i3 * t3
    where
        (n1:n2:n3:_) = face ^.. vertices . vertexNormal . _Just
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

main :: IO ()
main = do
    image <- thawImage $ makeImage 800 800 NC.black
    model <- loadWavefrontObj "data/african_head/african_head.obj"
    texture <- readImage TGA "data/african_head/african_head_diffuse.tga"

    let eye = Vector3 1 1 3
    let center = Vector3 0 0 0
    let up = Vector3 0 1 0

    let viewPort = viewportTransform (Vector2 100 100) 600 600 255
    let projection = identityMatrix 4 `mUpdate` [((4, 3), -1 / magnitudeVect eye)]
    let look = lookAt eye center up

    let lightingDirection = normalizeVect $ Vector3 1 (-1) 1
    let lightAt = lightIntensityAtPointOnFace lightingDirection

    putStrLn "Projection: "
    print projection
    putStrLn "View port: "
    print viewPort
    putStrLn "Look-at: "
    print look
    putStrLn "Transform: "
    print $ viewPort `mMult` projection `mMult` look

    drawTexturedModel image model (viewPort `mMult` projection `mMult` look) texture
        (\face point _ -> return $ scaleColor NC.white (lightAt face point))

    image' <- freezeImage image

    writeImage TGA "head.tga" image'

    putStrLn "Wakka wakka!"
