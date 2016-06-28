module LilRender.Color (RGBColor(..), scaleColor) where

import Control.DeepSeq
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

instance Storable RGBColor where
    sizeOf _ = 3 * sizeOf (undefined :: Word8)
    alignment _ = alignment (undefined :: Word8)
    peek ptr = do
        let ptr' = castPtr ptr :: Ptr Word8
        red <- peek ptr'
        green <- peek ptr'
        blue <- peek ptr'
        return $ RGBColor blue green red
    poke ptr (RGBColor blue green red) = do
        let ptr' = castPtr ptr :: Ptr Word8
        poke ptr' red
        poke ptr' green
        poke ptr' blue

instance Storable (Maybe RGBColor) where
    sizeOf _ = 4 * sizeOf (undefined :: Word8)
    alignment _ = alignment (undefined :: Word8)
    peek ptr = do
        let ptr' = castPtr ptr :: Ptr Word8
        isValid <- peek ptr'
        red <- peek ptr'
        green <- peek ptr'
        blue <- peek ptr'
        return $ if isValid > 0 then Just (RGBColor blue green red) else Nothing
    poke ptr (Just (RGBColor blue green red)) = do
        let ptr' = castPtr ptr :: Ptr Word8
        poke ptr' 1 -- it's valid
        poke ptr' red
        poke ptr' green
        poke ptr' blue
    poke ptr Nothing = do
        let ptr' = castPtr ptr :: Ptr Word8
        poke ptr' 0
        poke ptr' 0
        poke ptr' 0
        poke ptr' 0
