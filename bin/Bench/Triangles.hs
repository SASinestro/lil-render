module Bench.Triangles where

import           Criterion
import qualified LilRender.Color.Named             as NC
import           LilRender.Image
import           LilRender.Image.DrawingPrimitives
import           LilRender.Math.Geometry
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

triangleDrawEnv = do
    img <- thawImage $ makeImage 800 800 NC.black
    tri <- generate (arbitrary :: Gen (Triangle (Screen (Point3 Double))))
    return (img, tri)

benchTriangleDraw = env triangleDrawEnv $ bench "Draw triangle" . nfIO . \(image, tri) ->
    drawFilledTriangle image (\_ -> Just NC.red) tri :: IO ()

triangles = bgroup "Triangles" [ benchTriangleDraw ]
