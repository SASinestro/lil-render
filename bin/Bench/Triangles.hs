module Bench.Triangles where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Criterion
import qualified LilRender.Color.Named             as NC
import           LilRender.Image
import           LilRender.Image.DrawingPrimitives
import           LilRender.Image.Mutable
import           LilRender.Math.Geometry
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

instance Arbitrary (Screen (Point3 Double)) where
    arbitrary = do
        a <- choose (0, 800)
        b <- choose (0, 800)
        c <- choose (0, 800)
        return $ Screen (Point3 a b c)
instance (Arbitrary a) => Arbitrary (Triangle a) where
    arbitrary = do
        liftM3 Triangle arbitrary arbitrary arbitrary

triangleDrawEnv = do
    img <- thawImage $ makeImage 800 800 NC.black
    tri <- generate (arbitrary :: Gen (Triangle (Screen (Point3 Double))))
    return (img, tri)

benchTriangleDraw = env triangleDrawEnv $ bench "Draw triangle" . nfIO . \(image, tri) -> do
    drawFilledTriangle image (\_ -> Just NC.red) tri :: IO ()

triangles = bgroup "Triangles" [ benchTriangleDraw ]
