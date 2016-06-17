module Image.Color (RGBColor(..), scaleColor, transparentColor, blendColor) where

import Control.DeepSeq
import Data.Bits
import Data.Word
import GHC.Generics     (Generic)
import Text.Printf

import Foreign
import Foreign.Storable

data RGBColor = RGBColor {
      _red   :: !Word8
    , _green :: !Word8
    , _blue  :: !Word8
    , _alpha :: !Word8
} deriving (Eq, Generic)

instance NFData RGBColor

instance Storable RGBColor where
    sizeOf _ = 4
    alignment _ = 4

    {-# INLINE peek #-}
    peek ptr = do
        let wordPtr = castPtr ptr :: Ptr Word32
        word <- peek wordPtr

        let red   = fromIntegral $ word             .&. 0xFF :: Word8
        let green = fromIntegral $ word `shiftR` 8  .&. 0xFF :: Word8
        let blue  = fromIntegral $ word `shiftR` 16 .&. 0xFF :: Word8
        let alpha = fromIntegral $ word `shiftR` 24 .&. 0xFF :: Word8

        return $ RGBColor red green blue alpha

    poke ptr (RGBColor r g b a) = poke (castPtr ptr) word
        where
            r' = fromIntegral r :: Word32
            g' = fromIntegral g :: Word32
            b' = fromIntegral b :: Word32
            a' = fromIntegral a :: Word32
            word = r' + g' `shiftL` 8 + b' `shiftL` 16 + a' `shiftL` 24


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
