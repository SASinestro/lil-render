module Image.Color (RGBColor(..), red, green, blue, alpha, scaleColor, transparentColor, blendColor) where

import Control.Lens
import Data.Bits
import Data.Word
import Text.Printf

data RGBColor = RGBColor {
      _red   :: Word8
    , _green :: Word8
    , _blue  :: Word8
    , _alpha :: Word8
} deriving (Eq)

makeLenses ''RGBColor

instance Show RGBColor where
    show color = printf "(#%02X%02X%02X, %02f)" (_red color) (_green color) (_blue color) (fromIntegral (_alpha color) / 255 :: Double)

scaleColor :: RGBColor -> Double -> RGBColor
scaleColor (RGBColor r g b a) factor' = RGBColor r' g' b' a
    where
        factor = max 0 $ min 1 factor'
        r' = round (factor * fromIntegral r)
        g' = round (factor * fromIntegral g)
        b' = round (factor * fromIntegral b)

transparentColor :: RGBColor -> Double -> RGBColor
transparentColor (RGBColor r g b a) factor = RGBColor r g b a'
    where a' = round $ factor * fromIntegral a

blendColor :: RGBColor -> RGBColor -> RGBColor
blendColor (RGBColor r1 g1 b1 a') (RGBColor r2 g2 b2 _) = RGBColor (blend' r1 r2) (blend' g1 g2) (blend' b1 b2) 255
    where
        a = fromIntegral a' :: Int
        blend' s' d' = fromIntegral ((s * a + d * (255 - a)) `shiftR` 8)
            where
                s = fromIntegral s'
                d = fromIntegral d'
