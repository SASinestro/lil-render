module LilRender.Image.TGA (
      TGAHeader(..)
    , simpleTGAHeader
    , TGAImageType(..)
    , TGAOrigin(..)
    , TGADataStorage(..)
    , TGAImageDescriptor(..)
    , TGAImage(..)
    , readTGA
    , writeTGA
    , TGAImageData(..)
) where

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString       as BS
import           Data.Int
import qualified Data.Vector           as V
import           Data.Word
import           LilRender.Image.Color

import           Data.Store
import           Data.Store.Internal   (skip)
import           TH.Derive

data TGAHeader = TGAHeader {
      _tgaIdFieldLen      :: Word8
    , _tgaHasColorMap     :: Bool
    , _tgaImageType       :: TGAImageType
    , _tgaColorMapOffset  :: Word16
    , _tgaColorMapLength  :: Word16
    , _tgaColorMapDepth   :: Word8
    , _tgaX_origin        :: Word16
    , _tgaY_origin        :: Word16
    , _tgaWidth           :: Word16
    , _tgaHeight          :: Word16
    , _tgaBitDepth        :: Word8
    , _tgaImageDescriptor :: TGAImageDescriptor
} deriving (Show, Eq)

simpleTGAHeader :: (Integral a) => a -> a -> TGAHeader
-- Usually, it'll be an unindexed, uncompressed, 24-bit image being outputted
simpleTGAHeader width height = TGAHeader {
      _tgaIdFieldLen = 0
    , _tgaHasColorMap = False
    , _tgaImageType = TGAUncompressedRGBA
    , _tgaColorMapOffset = 0
    , _tgaColorMapLength = 0
    , _tgaColorMapDepth  = 0
    , _tgaX_origin = 0
    , _tgaY_origin = 0
    , _tgaWidth = fromIntegral width
    , _tgaHeight = fromIntegral height
    , _tgaBitDepth = 24
    , _tgaImageDescriptor = TGAImageDescriptor { _tgaAttributeBitsPerPixel = 0, _tgaScreenOrigin = TGALowerLeft, _tgaDataStorage = TGANonInterleaved }
}

data TGAImageType = TGAUncompressedIndexed | TGAUncompressedRGBA | TGARLEIndexed | TGARLERGBA deriving (Show, Eq)

instance Enum TGAImageType where
    fromEnum TGAUncompressedIndexed = 1
    fromEnum TGAUncompressedRGBA = 2
    fromEnum TGARLEIndexed = 9
    fromEnum TGARLERGBA = 10

    toEnum 1 = TGAUncompressedIndexed
    toEnum 2 = TGAUncompressedRGBA
    toEnum 9 = TGARLEIndexed
    toEnum 10 = TGARLERGBA

instance Store TGAImageType where
    size = ConstSize 1
    poke = poke . (fromIntegral :: Int -> Word8) . fromEnum
    peek = liftM (toEnum . (fromIntegral :: Word8 -> Int)) peek

data TGAOrigin = TGALowerLeft | TGAUpperLeft deriving (Show, Eq, Enum)

data TGADataStorage = TGANonInterleaved
                    | TGAEvenOddInterleaved
                    | TGAFourWayInterleaved
                    | TGAReservedStorageFlag
                    deriving (Show, Eq, Enum)

data TGAImageDescriptor = TGAImageDescriptor {
      _tgaAttributeBitsPerPixel :: Word8
    , _tgaScreenOrigin          :: TGAOrigin
    , _tgaDataStorage           :: TGADataStorage
} deriving (Show, Eq)

instance Store TGAImageDescriptor where
    size = ConstSize 1
    peek = do
        byte <- peek

        let bitsPerPixel = byte .&. 0xF -- 0xF = 0000 1111
        let origin = toEnum . fromIntegral . flip shiftR 5 $ byte .&. 0x20 -- 0x20 = 0010 0000
        let interleaving = toEnum . fromIntegral . flip shiftR 6 $ byte .&. 0xC0 -- 0xC0 = 1100 0000

        return $ TGAImageDescriptor bitsPerPixel origin interleaving
    poke (TGAImageDescriptor attributeBitsPerPixel screenOrigin dataStorage) = do
        let byte = attributeBitsPerPixel
                 + (flip shiftL 5 . fromIntegral . fromEnum $ screenOrigin)
                 + (flip shiftL 6 . fromIntegral . fromEnum $ dataStorage)

        poke byte

$($(derive [d| instance Deriving (Store TGAHeader) |]))

data TGAImage = TGAImage {
      _tgaHeader    :: TGAHeader
    , _tgaColorMap  :: TGAColorMap
    , _tgaImageData :: TGAImageData
} deriving (Show, Eq)

depthToBytes :: Int -> Int
depthToBytes  0 = 0
depthToBytes 15 = 2
depthToBytes 16 = 2
depthToBytes 24 = 3
depthToBytes 32 = 4
depthToBytes _  = error "Unsupported pixel format."

readTGA :: FilePath -> IO TGAImage
readTGA path = do
    contents <- BS.readFile path
    let (Right (TGAHeader {
          _tgaWidth = width'
        , _tgaHeight = height'
        , _tgaBitDepth = bitDepth'
        , _tgaColorMapLength = colorMapLength'
        , _tgaColorMapDepth = colorMapDepth'
        , _tgaColorMapOffset = colorMapOffset'
    })) = decode $ BS.take 18 {- Size of TGA header -} contents :: Either PeekException TGAHeader

    let width = fromIntegral width' :: Int
    let height = fromIntegral height' :: Int
    let bitDepth = fromIntegral bitDepth' :: Int
    let colorMapLength = fromIntegral colorMapLength' :: Int
    let colorMapDepth = fromIntegral colorMapDepth' :: Int
    let colorMapOffset = fromIntegral colorMapOffset' :: Int

    let size = fromIntegral (18 + colorMapOffset + (colorMapLength * depthToBytes colorMapDepth) + (width * height * depthToBytes bitDepth)) :: Int
    decodeIO $ BS.take size contents


writeTGA :: FilePath -> TGAImage -> IO ()
writeTGA path = BS.writeFile path . encode


type TGAColorMap = V.Vector RGBColor
type TGAColorMapIndex = Word8 -- At least for the files I'm looking at

data TGAImageData = TGAIndexedData { _indexedData :: V.Vector TGAColorMapIndex }
                  | TGAUnmappedData { _unindexedData :: V.Vector RGBColor }
                  deriving (Show, Eq)

-- THE REAL DARK SOULS BEGINS HERE
instance Store TGAImage where
    size = VarSize (\(TGAImage TGAHeader { _tgaWidth = width
                                           , _tgaHeight = height
                                           , _tgaHasColorMap = hasColorMap
                                           , _tgaColorMapOffset = colorMapOffset
                                           , _tgaColorMapLength = colorMapLength
                                           , _tgaColorMapDepth  = colorMapDepth
                                           , _tgaBitDepth = bitDepth
                                           }  _ _) ->
                                                18 + fromIntegral colorMapOffset
                                                   + (fromIntegral colorMapLength * depthToBytes (fromIntegral colorMapDepth))
                                                   + (fromIntegral width * fromIntegral height * (depthToBytes $ fromIntegral bitDepth)))

    peek = do
        header @ TGAHeader {
              _tgaWidth = width'
            , _tgaHeight = height'

            , _tgaHasColorMap = hasColorMap
            , _tgaColorMapOffset = colorMapOffset
            , _tgaColorMapLength = colorMapLength
            , _tgaColorMapDepth  = colorMapDepth
            , _tgaBitDepth = bitDepth
            } <- peek

        let width = fromIntegral width' :: Int
        let height = fromIntegral height' :: Int

        colorMap <- if hasColorMap
                            then do
                                skip $ fromIntegral colorMapOffset
                                V.replicateM (fromIntegral colorMapLength) $ peekColor colorMapDepth
                            else return V.empty

        imageData <- if hasColorMap
                            then liftM TGAIndexedData $ V.replicateM (width * height) peek
                            else liftM TGAUnmappedData $ V.replicateM (width * height) (peekColor bitDepth)

        return $ TGAImage header colorMap imageData
        where
            -- Don't ask me, I'm just a girl!
            fiveBitToEightBit five = (five * 527 + 23) `shiftR` 6

            peekColor :: Word8 -> Peek RGBColor
            peekColor 15 = do
                byte1 <- peek
                byte2 <- peek

                let blue' = byte1 .&. 0x1F
                let green' = ((byte2 `shiftL` 3) .&. 0x1C) .|. ((byte1 `shiftR` 5) .&. 0x07)
                let red' = (byte2 `shiftR` 2) .&. 0x1F

                let alpha = 255
                let red = fiveBitToEightBit red'
                let green = fiveBitToEightBit green'
                let blue = fiveBitToEightBit blue'
                return $ RGBColor red green blue alpha
            peekColor 16 = do
                byte1 <- peek
                byte2 <- peek

                let blue' = byte1 .&. 0x1F
                let green' = ((byte2 `shiftL` 3) .&. 0x1C) .|. ((byte1 `shiftR` 5) .&. 0x07)
                let red' = (byte2 `shiftR` 2) .&. 0x1F

                let alpha = 255 * (byte2 .&. 0x80) `shiftR` 7
                let red = fiveBitToEightBit red'
                let green = fiveBitToEightBit green'
                let blue = fiveBitToEightBit blue'
                return $ RGBColor red green blue alpha
            peekColor 24 = do
                blue <- peek
                green <- peek
                red <- peek
                return $ RGBColor red green blue 255
            peekColor 32 = do
                blue <- peek
                green <- peek
                red <- peek
                alpha <- peek
                return $ RGBColor red green blue alpha
            peekColor _ = fail "Unsupported pixel format."

    poke (TGAImage {
          _tgaHeader = tgaHeader @ (TGAHeader {
                _tgaHasColorMap = hasColorMap
              , _tgaColorMapOffset = colorMapOffset
              , _tgaBitDepth = bitDepth
          })
        , _tgaColorMap = colorMap
        , _tgaImageData = imageData
    }) = do
        poke tgaHeader

        replicateM_ (fromIntegral colorMapOffset) $ poke (0 :: Word8)

        when hasColorMap $ mapM_ (pokeColor bitDepth) colorMap

        pokeImageData bitDepth imageData

        where
            -- 'what the fuck?' is both appropriate and a good reference here!
            eightBitToFiveBit :: Word8 -> Word16 -- Bullshit to make the types line up right, don't question it.
            eightBitToFiveBit eight = ((fromIntegral eight :: Word16) * 249 + 1014) `shiftR` 11

            pokeColor :: Word8 -> RGBColor -> Poke ()
            pokeColor 15 (RGBColor r g b _) = do
                let red' = eightBitToFiveBit r
                let green' = eightBitToFiveBit g
                let blue' = eightBitToFiveBit b

                let outpoke = red' + (green' `shiftL` 5) + (blue' `shiftL` 10)
                poke outpoke
            pokeColor 16 (RGBColor r g b a) = do
                let red' = eightBitToFiveBit r
                let green' = eightBitToFiveBit g
                let blue' = eightBitToFiveBit b
                let alpha' = if a >= 128 then 1 else 0

                let outpoke = red' + (green' `shiftL` 5) + (blue' `shiftL` 10) + (alpha' `shiftL` 15)
                poke outpoke
            pokeColor 24 (RGBColor r g b _) = do
                poke b
                poke g
                poke r
            pokeColor 32 (RGBColor r g b a) = do
                poke b
                poke g
                poke r
                poke a
            pokeColor _ _ = fail "Unsupported pixel format."

            pokeImageData :: Word8 -> TGAImageData -> Poke ()
            pokeImageData _ (TGAIndexedData indexes) = mapM_ poke indexes
            pokeImageData depth (TGAUnmappedData colors) = mapM_ (pokeColor depth) colors
