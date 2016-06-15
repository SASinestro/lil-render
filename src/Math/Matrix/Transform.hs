module Math.Matrix.Transform where

import Data.Foldable
import Math.Matrix
import Math.Vector

vectorToMatrix :: (VectorMath v Double, Foldable v) => v Double -> Matrix Double
vectorToMatrix v = matrixFrom2DList [toList v ++ [1]]

matrixTo2DVector :: Matrix Double -> Vector2 Double
matrixTo2DVector mat
    | _mCols mat == 3 && _mRows mat == 1 = Vector2 (a / c) (b / c)
    | otherwise = error "Non 3x1 matrices cannot be converted into 2D coordinates."
        where
            a = mat `mIndex` (1, 1)
            b = mat `mIndex` (2, 1)
            c = mat `mIndex` (3, 1)

matrixTo3DVector :: Matrix Double -> Vector3 Double
matrixTo3DVector mat
    | _mCols mat == 4 && _mRows mat == 1 = Vector3 (a / d) (b / d) (c / d)
    | otherwise = error "Non 4x1 matrices cannot be converted into 3D coordinates."
        where
            a = mat `mIndex` (1, 1)
            b = mat `mIndex` (2, 1)
            c = mat `mIndex` (3, 1)
            d = mat `mIndex` (4, 1)

viewportTransform :: Vector2 Int -> Int -> Int -> Int -> Matrix Double
viewportTransform (Vector2 x' y') width height depth =
    identityMatrix 4 `mUpdate` [ ((1, 4), x + w / 2),
                                 ((2, 4), y + h / 2),
                                 ((3, 4),     d / 2),
                                 ((1, 1),     w / 2),
                                 ((2, 2),     h / 2),
                                 ((3, 3),     d / 2) ]
    where
        w = fromIntegral width  :: Double
        h = fromIntegral height :: Double
        d = fromIntegral depth  :: Double

        x = fromIntegral x'
        y = fromIntegral y'

translationTransform :: Double -> Double -> Double -> Matrix Double
translationTransform dx dy dz =
    identityMatrix 4 `mUpdate` [((1, 4), dx), ((2, 4), dy), ((3, 4), dz)]

zoomTransform :: Double -> Matrix Double
zoomTransform factor = identityMatrix 4 `mUpdate` [((i, i), factor) | i <- [1, 3]]

xRotateTransform :: Double -> Matrix Double
xRotateTransform angle =
    identityMatrix 4 `mUpdate` [((2, 2), cos angle), ((3, 3), cos angle), ((2, 3), -sin angle), ((3, 2), sin angle)]

yRotateTransform :: Double -> Matrix Double
yRotateTransform angle =
    identityMatrix 4 `mUpdate` [((1, 1), cos angle), ((3, 3), cos angle), ((1, 3), sin angle), ((3, 1), -sin angle)]

zRotateTransform :: Double -> Matrix Double
zRotateTransform angle =
    identityMatrix 4 `mUpdate` [((1, 1), cos angle), ((2, 2), cos angle), ((1, 2), -sin angle), ((2, 1), sin angle)]

