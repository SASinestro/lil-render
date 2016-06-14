module Image.Color (RGBColor(..), red, green, blue, alpha, scaleColor) where

import Control.Lens
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
scaleColor (RGBColor r g b a) factor = RGBColor r' g' b' a
    where
        r' = round (factor * fromIntegral r)
        g' = round (factor * fromIntegral g)
        b' = round (factor * fromIntegral b)