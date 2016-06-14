module Image.Drawing.Primitives (drawLine, drawTriangle, drawFilledTriangle) where

import Control.Monad
import Control.Monad.Primitive
import Data.List
import Data.Ord
import Image                   (ImageIndexType)
import Image.Color
import Image.Mutable
import Math.Vector

drawLine :: (PrimMonad m) => MutableImage (PrimState m) -> RGBColor -> ImageIndexType -> ImageIndexType -> m ()
drawLine img color (x0, y0) (x1, y1)
    | abs (x0 - x1) < abs (y0 - y1) = mapM_ (drawPixel img color) $ transpose <$> drawLine' (y0, x0) (y1, x1)
    | otherwise = mapM_ (drawPixel img color) $ drawLine' (x0, y0) (x1, y1)
    where
        transpose :: (a, b) -> (b, a)
        transpose (a, b) = (b, a)

        drawLine' :: ImageIndexType -> ImageIndexType -> [ImageIndexType]
        drawLine' (x0, y0) (x1, y1)
            | x0 > x1 = drawLine' (x1, y1) (x0, y0)
            | otherwise = do
                x <- [x0 .. x1]
                let realX = fromIntegral x :: Double

                let t = (realX - realX0) / (realX1 - realX0)
                let y = round ((1.0 - t) * realY0 + t * realY1)

                return (x, y)
            where
                realX0 = fromIntegral x0 :: Double
                realY0 = fromIntegral y0 :: Double
                realX1 = fromIntegral x1 :: Double
                realY1 = fromIntegral y1 :: Double

drawTriangle :: (PrimMonad m) => MutableImage (PrimState m) -> RGBColor
                -> (ImageIndexType, ImageIndexType, ImageIndexType) -> m ()
drawTriangle img color (v1, v2, v3) = do
    drawLine img color v1 v2
    drawLine img color v2 v3
    drawLine img color v3 v1

drawFilledTriangle :: (PrimMonad m) => MutableImage (PrimState m) -> RGBColor
                      -> (ImageIndexType, ImageIndexType, ImageIndexType) -> m ()
drawFilledTriangle img color ((v1x, v1y), (v2x, v2y), (v3x, v3y)) =
    mapM_ (drawPixel img color) [(i, j) | i <- [minX .. maxX], j <- [minY .. maxY], isInTriangle (i, j)]
    where
        minX = max 0                  $ minimum [v1x, v2x, v3x]
        maxX = min (_mWidth img - 1)  $ maximum [v1x, v2x, v3x]

        minY = max 0                  $ minimum [v1y, v2y, v3y]
        maxY = min (_mHeight img - 1) $ maximum [v1y, v2y, v3y]

        toBarycentric :: ImageIndexType -> Maybe (Vector3 Double)
        toBarycentric (px, py) = barycentric' $ crossVect (fromIntegral <$> vect1) (fromIntegral <$> vect2)
            where
                vect1 = Vector3 (v3x - v1x) (v2x - v1x) (v1x - px)
                vect2 = Vector3 (v3y - v1y) (v2y - v1y) (v1y - py)
                barycentric' :: Vector3 Double -> Maybe (Vector3 Double)
                barycentric' (Vector3 a b c)
                    | abs c < 1 = Nothing
                    | otherwise = Just $ Vector3 (1 - (a + b)/c) (b/c) (a/c)

        isInTriangle :: ImageIndexType -> Bool
        isInTriangle = maybe False (\(Vector3 a b c) -> a > 0 && b > 0 && c > 0) . toBarycentric
