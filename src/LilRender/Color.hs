module LilRender.Color (RGBColor(..), scaleColor) where

import Control.DeepSeq
import Data.Bits
import Data.Vector.Unboxed.Deriving
import Data.Word
import GHC.Generics                 (Generic)
import Text.Printf

data RGBColor = RGBColor {
      _blue  :: !Word8
    , _green :: !Word8
    , _red   :: !Word8
} deriving (Eq, Generic)

instance NFData RGBColor

instance Show RGBColor where
    show color = printf "(#%02X%02X%02X)" (_red color) (_green color) (_blue color)

scaleColor ∷ RGBColor → Double → RGBColor
scaleColor (RGBColor r g b) factor = RGBColor (round r') (round g') (round b')
    where
        r' = factor * fromIntegral r
        g' = factor * fromIntegral g
        b' = factor * fromIntegral b

--

colorToWord32 :: RGBColor -> Word32
colorToWord32 (RGBColor r g b) = r' + g' + b'
    where
        b' = fromIntegral b :: Word32
        g' = fromIntegral g `shiftL` 8
        r' = fromIntegral r `shiftL` 16

word32ToColor :: Word32 -> RGBColor
word32ToColor word = RGBColor r g b
    where
        b = fromIntegral (word .&. 0xFF)
        g = fromIntegral (word `shiftR` 8  .&. 0xFF)
        r = fromIntegral (word `shiftR` 16 .&. 0xFF)

derivingUnbox "VertexPoint"
    [t| RGBColor -> Word32 |]
    [| colorToWord32 |]
    [| word32ToColor |]
