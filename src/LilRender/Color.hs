module LilRender.Color (RGBColor(..), scaleColor, colorToWord32, word32ToColor) where

import Control.Monad
import Control.DeepSeq
import Data.Bits
import Data.Word
import GHC.Generics                 (Generic)
import Foreign.Ptr
import Foreign.Storable
import Text.Printf

data RGBColor = RGBColor {
      _blue  :: !Word8
    , _green :: !Word8
    , _red   :: !Word8
} deriving (Eq, Generic)

instance NFData RGBColor

instance Show RGBColor where
    show (RGBColor blue green red) = printf "(#%02X%02X%02X)" red green blue

scaleColor ∷ RGBColor → Double → RGBColor
scaleColor (RGBColor b g r) factor = RGBColor (round r') (round g') (round b')
    where
        factor' = max 0 $ min 1 factor
        r' = factor' * fromIntegral r
        g' = factor' * fromIntegral g
        b' = factor' * fromIntegral b

--

colorToWord32 :: RGBColor -> Word32
colorToWord32 (RGBColor b g r) = r' + g' + b'
    where
        b' = fromIntegral b :: Word32
        g' = fromIntegral g `shiftL` 8
        r' = fromIntegral r `shiftL` 16

word32ToColor :: Word32 -> RGBColor
word32ToColor word = RGBColor b g r
    where
        b = fromIntegral (word .&. 0xFF)
        g = fromIntegral (word `shiftR` 8  .&. 0xFF)
        r = fromIntegral (word `shiftR` 16 .&. 0xFF)

instance Storable RGBColor where
    sizeOf _ = sizeOf (undefined :: Word32)
    alignment _ = alignment (undefined :: Word32)
    peek ptr = do
        let ptr' = castPtr ptr :: Ptr Word32
        liftM word32ToColor (peek ptr')
    poke ptr color = do
        let ptr' = castPtr ptr :: Ptr Word32
        poke ptr' (colorToWord32 color)

instance Storable (Maybe RGBColor) where
    sizeOf _ = sizeOf (undefined :: Word32)
    alignment _ = alignment (undefined :: Word32)
    peek ptr = do
        let ptr' = castPtr ptr :: Ptr Word32
        word32 <- peek ptr'
        if word32 <= 0xFFFFFF then return . Just . word32ToColor $ word32 else return Nothing
    poke ptr (Just color) = do
        let ptr' = castPtr ptr :: Ptr Word32
        poke ptr' (colorToWord32 color)
    poke ptr Nothing = do
        let ptr' = castPtr ptr :: Ptr Word32
        poke ptr' (0x1000000)
