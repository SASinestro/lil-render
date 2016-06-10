module Image.Color (RGBColor(..), red, green, blue, alpha) where

import Control.Lens
import Data.Word

data RGBColor = RGBColor {
      _red :: Int
    , _green :: Int
    , _blue :: Int
    , _alpha :: Double
} deriving (Eq, Show)

makeLenses ''RGBColor