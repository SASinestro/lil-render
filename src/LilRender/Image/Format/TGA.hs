module LilRender.Image.Format.TGA (
      TGAHeader(..)
    , simpleTGAHeader
    , TGAImageType(..)
    , TGAOrigin(..)
    , TGADataStorage(..)
    , TGAImageDescriptor(..)
    , TGAImage(..)
    , readTGA
    , readTGAIO
    , writeTGA
    , TGAImageData(..)
) where

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString           as BS
import qualified Data.Vector.Storable      as V
import           Data.Word

import           LilRender.Color
import           LilRender.Image.Immutable

import           Data.Store
import           Data.Store.Internal       (skip)
import           TH.Derive

import Debug.Trace

data TGAHeader = TGAHeader {
      _tgaIdFieldLen      :: Word8
    , _tgaHasColorMap     :: Bool
    , _tgaImageType       :: TGAImageType
    , _tgaColorMapOffset  :: Word16
    , _tgaColorMapLength  :: Word16
    , _tgaColorMapDepth   :: Word8
    , _tgaXOrigin         :: Word16
    , _tgaYOrigin         :: Word16
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
    , _tgaXOrigin = 0
    , _tgaYOrigin = 0
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
depthToBytes 24 = 3
depthToBytes _  = error "Unsupported pixel format."

readTGAIO :: FilePath -> IO TGAImage
readTGAIO path = do
    contents <- BS.readFile path
    return $ readTGA contents

readTGA :: BS.ByteString -> TGAImage
readTGA contents = image
    where
        (Right (TGAHeader {
              _tgaWidth = width'
            , _tgaHeight = height'
            , _tgaBitDepth = bitDepth'
            , _tgaColorMapLength = colorMapLength'
            , _tgaColorMapDepth = colorMapDepth'
            , _tgaColorMapOffset = colorMapOffset'
        })) = decode $ BS.take 18 {- Size of TGA header -} contents :: Either PeekException TGAHeader

        width = fromIntegral width' :: Int
        height = fromIntegral height' :: Int
        bitDepth = fromIntegral bitDepth' :: Int
        colorMapLength = fromIntegral colorMapLength' :: Int
        colorMapDepth = fromIntegral colorMapDepth' :: Int
        colorMapOffset = fromIntegral colorMapOffset' :: Int

        size = fromIntegral (18 + colorMapOffset + (colorMapLength * depthToBytes colorMapDepth) + (width * height * depthToBytes bitDepth)) :: Int
        (Right image) = decode $ BS.take size contents


writeTGA :: FilePath -> TGAImage -> IO ()
writeTGA path = do
    traceM "writeTGA"
    BS.writeFile path . encode

instance ImageConvertible TGAImage where
    toImage (TGAImage TGAHeader { _tgaHeight = height, _tgaWidth = width } color_map image_data) = Image storage height' width'
        where
            height' = fromIntegral height
            width'  = fromIntegral width
            storage = case image_data of
                (TGAIndexedData indexes) -> V.map (V.unsafeIndex color_map . fromIntegral) indexes
                (TGAUnmappedData colors) -> colors
    fromImage (Image storage width height) = TGAImage {
          _tgaHeader    = simpleTGAHeader width height
        , _tgaColorMap  = V.empty
        , _tgaImageData = TGAUnmappedData storage
    }


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
            peekColor :: Word8 -> Peek RGBColor
            peekColor 24 = do
                blue <- peek
                green <- peek
                red <- peek
                return $ RGBColor red green blue
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

        when hasColorMap $ V.mapM_ (pokeColor bitDepth) colorMap

        pokeImageData bitDepth imageData

        where
            pokeColor :: Word8 -> RGBColor -> Poke ()
            pokeColor 24 (RGBColor b g r) = do
                poke b
                poke g
                poke r
            pokeColor _ _ = fail "Unsupported pixel format."

            pokeImageData :: Word8 -> TGAImageData -> Poke ()
            pokeImageData _ (TGAIndexedData indexes) = V.mapM_ poke indexes
            pokeImageData depth (TGAUnmappedData colors) = V.mapM_ (pokeColor depth) colors
