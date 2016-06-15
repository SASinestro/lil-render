module Image.Drawing.Primitives (drawFilledTriangle, Triangle) where

import Control.Monad
import Control.Monad.Primitive
import Data.List
import Data.Maybe
import Data.Ord
import Image
import Image.Color
import Image.Mutable
import Math.Vector
import Model

type Triangle = (Vector3 Int, Vector3 Int, Vector3 Int)

drawFilledTriangle :: (PrimMonad m) => MutableImage (PrimState m) -> (Vector3 Double -> m RGBColor) -> Triangle  -> m ()
drawFilledTriangle img getColor (Vector3 v1x v1y v1z, Vector3 v2x v2y v2z, Vector3 v3x v3y v3z) =
    mapM_ (\(c', x, y, z) -> do
        c <- c'
        drawPixel img c (x, y, z)) [(color, i, j, k) | i <- [minX .. maxX], j <- [minY .. maxY],
                                                       let b = toBarycentric (i, j),
                                                       let color  = getColor b,
                                                       let k = getZ b,
                                                       isInTriangle b ]
    where
        minX = max 0                  $ minimum [v1x, v2x, v3x]
        maxX = min (_mWidth img - 1)  $ maximum [v1x, v2x, v3x]

        minY = max 0                  $ minimum [v1y, v2y, v3y]
        maxY = min (_mHeight img - 1) $ maximum [v1y, v2y, v3y]

        toBarycentric :: ImageIndexType -> Vector3 Double
        toBarycentric (px, py)
            | abs c < 1 = Vector3 (-1) 1 1
            | otherwise = Vector3 (1 - (a + b)/c) (b/c) (a/c)
            where
                vect1 = Vector3 (v3x - v1x) (v2x - v1x) (v1x - px)
                vect2 = Vector3 (v3y - v1y) (v2y - v1y) (v1y - py)
                (Vector3 a b c) = crossVect (fromIntegral <$> vect1) (fromIntegral <$> vect2)

        getZ :: Vector3 Double -> Int
        getZ (Vector3 b1 b2 b3) = floor (b1 * fromIntegral v1z + b2 * fromIntegral v2z + b3 * fromIntegral v3z)

        isInTriangle :: Vector3 Double -> Bool
        isInTriangle (Vector3 a b c) = a >= 0 && b >= 0 && c >= 0
