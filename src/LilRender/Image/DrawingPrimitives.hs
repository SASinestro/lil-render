module LilRender.Image.DrawingPrimitives (drawFilledTriangle, Triangle) where

import Control.Monad           (when)
import Control.Monad.Primitive

import LilRender.Color
import LilRender.Image.Mutable
import LilRender.Math.Geometry

boundingRect :: Triangle (Screen (Point3 Double)) -> [Screen (Point2 Int)]
boundingRect (Triangle (Screen (Point3 p1x p1y _)) (Screen (Point3 p2x p2y _)) (Screen (Point3 p3x p3y _))) =
    [Screen (Point2 i j) | i <- [minX .. maxX], j <- [minY .. maxY]]
    where
        minX = round $ minimum [p1x, p2x, p3x]
        maxX = round $ maximum [p1x, p2x, p3x]

        minY = round $ minimum [p1y, p2y, p3y]
        maxY = round $ maximum [p1y, p2y, p3y]

isInTriangle :: Barycentric (Point3 Double) -> Bool
isInTriangle (Barycentric (Point3 a b c)) = a >= 0 && b >= 0 && c >= 0

drawFilledTriangle :: forall m. (PrimMonad m) => MutableImage (PrimState m)
                                              -> (Barycentric (Point3 Double) -> Maybe RGBColor)
                                              -> Triangle (Screen (Point3 Double))
                                              -> m ()
drawFilledTriangle img getColor triangle @ (Triangle (Screen (Point3 _ _ z1))
                                                     (Screen (Point3 _ _ z2))
                                                     (Screen (Point3 _ _ z3 ))) =
    mapM_ drawPointInTriangle $ boundingRect triangle

    where
        addZ :: Screen (Point2 Int) -> Barycentric (Point3 Double) -> Screen (Point3 Int)
        addZ (Screen (Point2 x y)) bary = Screen $ Point3 x y (round $ triangularInterpolate z1 z2 z3 bary)

        drawPointInTriangle :: Screen (Point2 Int) -> m ()
        drawPointInTriangle point = when (isInTriangle bary) $ maybe (return ()) (drawPixel img (addZ point bary)) $ getColor bary
            where bary = toBarycentric triangle (fmap fromIntegral <$> point)
