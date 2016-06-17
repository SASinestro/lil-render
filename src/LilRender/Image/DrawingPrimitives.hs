module LilRender.Image.DrawingPrimitives (drawFilledTriangle, Triangle) where

import Control.Arrow
import Control.Monad
import Control.Monad.Primitive
import Data.Ix
import Data.List
import Data.Maybe
import Data.Ord

import LilRender.Image
import LilRender.Image.Color
import LilRender.Image.Mutable
import LilRender.Math.Geometry
import LilRender.Math.Vector
import LilRender.Model


-- .. for Doubles doesn't really work.
boundingRect :: Triangle (Screen (Point3 Double)) -> [Screen (Point2 Int)]
boundingRect (Triangle (Screen (Point3 p1x p1y _)) (Screen (Point3 p2x p2y _)) (Screen (Point3 p3x p3y _))) = [Screen (Point2 i j) | i <- [minX .. maxX], j <- [minY .. maxY]]
    where
        minX = round $ minimum [p1x, p2x, p3x]
        maxX = round $ maximum [p1x, p2x, p3x]

        minY = round $ minimum [p1y, p2y, p3y]
        maxY = round $ maximum [p1y, p2y, p3y]

isInTriangle :: Barycentric (Point3 Double) -> Bool
isInTriangle (Barycentric (Point3 a b c)) = a >= 0 && b >= 0 && c >= 0

drawFilledTriangle :: forall m. (PrimMonad m) => MutableImage (PrimState m) -> (Barycentric (Point3 Double) -> m RGBColor) -> Triangle (Screen (Point3 Double)) -> m ()
drawFilledTriangle img getColor triangle =
    mapM_ (\(screen, bary) -> getColor bary >>= drawPixel img (addZ screen bary)) $ filter (isInTriangle . snd) . fmap (\a -> (a, toBarycentric triangle $ (fmap . fmap) fromIntegral a)) $ boundingRect triangle
    where
        addZ :: Screen (Point2 Int) -> Barycentric (Point3 Double) -> Screen (Point3 Int)
        addZ (Screen (Point2 x y)) bary = Screen $ Point3 x y (round $ triangularInterpolate (fmap (_p3_z . fromScreen) triangle) bary)
