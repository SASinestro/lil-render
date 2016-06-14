module Image.Color (RGBColor(..), red, green, blue, alpha, scaleColor, blendColor) where

import Control.Lens
import Data.Word
import Text.Printf

data RGBColor = RGBColor {
      _red   :: Word8
    , _green :: Word8
    , _blue  :: Word8
    , _alpha :: Word8
} deriving (Eq)

instance Show RGBColor where
    show color = printf "(#%02X%02X%02X, %02f)" (_red color) (_green color) (_blue color) (fromIntegral (_alpha color) / 255 :: Double)

scaleColor :: RGBColor -> Double -> RGBColor
scaleColor (RGBColor r g b a) factor = RGBColor r' g' b' a
    where
        r' = round (factor * fromIntegral r)
        g' = round (factor * fromIntegral g)
        b' = round (factor * fromIntegral b)

blendColor :: RGBColor -> RGBColor -> RGBColor
blendColor (RGBColor bgRed bgGreen bgBlue _) (RGBColor fgRed fgGreen fgBlue a) = RGBColor red green blue 255
    where
        alpha = fromIntegral a / 255 :: Double

        fgRed' = fromIntegral fgRed
        fgGreen' = fromIntegral fgGreen
        fgBlue' = fromIntegral fgBlue

        bgRed' = fromIntegral bgRed
        bgGreen' = fromIntegral bgGreen
        bgBlue' = fromIntegral bgBlue

        red' = fgRed' * alpha + bgRed' * (1 - alpha)
        green' = fgGreen' * alpha + bgGreen' * (1 - alpha)
        blue' = fgBlue' * alpha + bgGreen' * (1 - alpha)

        red = round red'
        green = round green'
        blue = round blue'

makeLenses ''RGBColor
