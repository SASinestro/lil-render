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
        red   <- peekElemOff ptr' 0
        green <- peekElemOff ptr' 1
        blue  <- peekElemOff ptr' 2
        return $ RGBColor blue green red
    poke ptr (RGBColor blue green red) = do
        let ptr' = castPtr ptr :: Ptr Word8
        pokeElemOff ptr' 0 red
        pokeElemOff ptr' 1 green
        pokeElemOff ptr' 2 blue

instance Storable (Maybe RGBColor) where
    sizeOf _ = 4 * sizeOf (undefined :: Word8)
    alignment _ = alignment (undefined :: Word8)
    peek ptr = do
        let ptr' = castPtr ptr :: Ptr Word8
        isValid <- peekElemOff ptr' 0
        red     <- peekElemOff ptr' 1
        green   <- peekElemOff ptr' 2
        blue    <- peekElemOff ptr' 3
        return $ if isValid > 0 then Just (RGBColor blue green red) else Nothing
    poke ptr (Just (RGBColor blue green red)) = do
        let ptr' = castPtr ptr :: Ptr Word8
        pokeElemOff ptr' 0 1 -- it's valid
        pokeElemOff ptr' 1 red
        pokeElemOff ptr' 2 green
        pokeElemOff ptr' 3 blue
    poke ptr Nothing = do
        let ptr' = castPtr ptr :: Ptr Word8
        pokeElemOff ptr' 0 0 -- Four zero bytes = Nothing
        pokeElemOff ptr' 1 0
        pokeElemOff ptr' 2 0
        pokeElemOff ptr' 3 0
