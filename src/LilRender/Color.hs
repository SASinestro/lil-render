module LilRender.Color (RGBColor(..), scaleColor, transparentColor, blendColor) where

import Control.DeepSeq
import Data.Bits
import Data.Vector.Unboxed.Deriving
import Data.Word
import GHC.Generics                 (Generic)
import Text.Printf

data RGBColor = RGBColor {
      _red   :: !Word8
    , _green :: !Word8
    , _blue  :: !Word8
    , _alpha :: !Word8
} deriving (Eq, Generic)

instance NFData RGBColor

instance Show RGBColor where
    show color = printf "(#%02X%02X%02X, %02f)" (_red color) (_green color) (_blue color) (fromIntegral (_alpha color) / 255 :: Double)

scaleColor ∷ RGBColor → Double → RGBColor
scaleColor (RGBColor r g b a) factor = RGBColor (round r') (round g') (round b') (round a')
    where
        r' = factor * fromIntegral r
        g' = factor * fromIntegral g
        b' = factor * fromIntegral b
        a' = factor * fromIntegral a

transparentColor ∷ RGBColor → Double → RGBColor
transparentColor (RGBColor r g b a) factor = RGBColor (round r') (round g') (round b') (round factor)
    where
        r' = factor / a' * fromIntegral r
        g' = factor / a' * fromIntegral g
        b' = factor / a' * fromIntegral b
        a' = fromIntegral a

blendColor ∷ RGBColor → RGBColor → RGBColor
blendColor (RGBColor r1 g1 b1 a') (RGBColor r2 g2 b2 _) = RGBColor (blend' r1 r2) (blend' g1 g2) (blend' b1 b2) 255
    where
        a = fromIntegral a' / 255 :: Double
        blend' :: Word8 -> Word8 -> Word8
        blend' s' d' = round (s + d * a)
            where
                s = fromIntegral s'
                d = fromIntegral d'

--

colorToWord32 :: RGBColor -> Word32
colorToWord32 (RGBColor r g b a) = r' + g' + b' + a'
    where
        b' = fromIntegral b :: Word32
        g' = fromIntegral g `shiftL` 8
        r' = fromIntegral r `shiftL` 16
        a' = fromIntegral a `shiftL` 24

word32ToColor :: Word32 -> RGBColor
word32ToColor word = RGBColor r g b a
    where
        b = fromIntegral (word .&. 0xFF)
        g = fromIntegral (word `shiftR` 8  .&. 0xFF)
        r = fromIntegral (word `shiftR` 16 .&. 0xFF)
        a = fromIntegral (word `shiftR` 32 .&. 0xFF)

derivingUnbox "VertexPoint"
    [t| RGBColor -> Word32 |]
    [| colorToWord32 |]
    [| word32ToColor |]
