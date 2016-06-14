module Image.Drawing.Primitives (drawFilledTriangle) where

import Data.Maybe
import Control.Monad
import Control.Monad.Primitive
import Data.List
import Data.Ord
import Image                   (ImageIndexType)
import Image.Color
import Image.Mutable
import Math.Vector

drawFilledTriangle :: (PrimMonad m) => MutableImage (PrimState m) -> RGBColor
                      -> (Vector3 Double, Vector3 Double, Vector3 Double) -> m ()
drawFilledTriangle img color (Vector3 v1x' v1y' v1z', Vector3 v2x' v2y' v2z', Vector3 v3x' v3y' v3z') =
    mapM_ (drawPixel img color) [(i, j, z) | i <- [minX .. maxX], j <- [minY .. maxY],
                                          let (Vector3 bx by bz) = toBarycentric (i, j),
                                          isInTriangle (Vector3 bx by bz),
                                          let z = v1z' * bx + v2z' * by + v3z' * bz]
    where
        w = fromIntegral $ _mWidth img :: Double
        h = fromIntegral $ _mHeight img :: Double

        v1x = round ((v1x' + 1) * w / 2)
        v1y = round ((v1y' + 1) * h / 2)

        v2x = round ((v2x' + 1) * w / 2)
        v2y = round ((v2y' + 1) * h / 2)

        v3x = round ((v3x' + 1) * w / 2)
        v3y = round ((v3y' + 1) * h / 2)

        minX = max 0                  $ minimum [v1x, v2x, v3x]
        maxX = min (_mWidth img - 1)  $ maximum [v1x, v2x, v3x]

        minY = max 0                  $ minimum [v1y, v2y, v3y]
        maxY = min (_mHeight img - 1) $ maximum [v1y, v2y, v3y]

        toBarycentric :: ImageIndexType -> Vector3 Double
        toBarycentric (px, py) = fromMaybe (Vector3 (-1) 1 1) $ barycentric'
                                 $ crossVect (fromIntegral <$> vect1) (fromIntegral <$> vect2)
            where
                vect1 = Vector3 (v3x - v1x) (v2x - v1x) (v1x - px)
                vect2 = Vector3 (v3y - v1y) (v2y - v1y) (v1y - py)
                barycentric' :: Vector3 Double -> Maybe (Vector3 Double)
                barycentric' (Vector3 a b c)
                    | abs c < 1 = Nothing
                    | otherwise = Just $ Vector3 (1 - (a + b)/c) (b/c) (a/c)

        isInTriangle :: Vector3 Double -> Bool
        isInTriangle (Vector3 a b c) = a > 0 && b > 0 && c > 0
