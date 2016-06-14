module Image.Drawing.Primitives (drawLine, drawLine') where

import Control.Monad
import Control.Monad.Primitive

import Image                   (ImageIndexType)
import Image.Color
import Image.Mutable

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
