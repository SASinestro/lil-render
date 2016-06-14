module Image.Color (RGBColor(..), red, green, blue, alpha) where

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
    show color = printf "(#%02X%02X%02X, %02f)" (_red color) (_green color) (_blue color) ((fromIntegral $ _alpha color) / 255 :: Double)

makeLenses ''RGBColor
