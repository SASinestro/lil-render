module Image.TGA (
      LittleEndian(..)
    , TGAHeader(..)
    , simple_tga_header
    , TGAImageType(..)
    , TGAOrigin(..)
    , TGADataStorage(..)
    , TGAImageDescriptor(..)
    , TGAImage(..)
    , read_tga
    , write_tga
    , TGAImageData(..)
    -- Lenses
    , tga_id_field_len
    , tga_has_color_map
    , tga_image_type
    , tga_color_map_offset
    , tga_color_map_length
    , tga_color_map_depth
    , tga_x_origin
    , tga_y_origin
    , tga_width
    , tga_height
    , tga_bit_depth
    , tga_image_descriptor
    , tga_attribute_bits_per_pixel
    , tga_screen_origin
    , tga_data_storage
    , tga_header
    , tga_color_map
    , tga_image_data
) where

import           Control.Lens
import           Control.Monad
import           Data.Bits
import           Data.Int
import qualified Data.Vector     as V
import           Data.Word
import           GHC.Generics
import           Image.Color

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put


getInt16le = liftM (fromIntegral :: Word16 -> Int16) getWord16le
getInt32le = liftM (fromIntegral :: Word32 -> Int32) getWord32le

putInt16le = putWord16le . (fromIntegral :: Int16 -> Word16)
putInt32le = putWord32le . (fromIntegral :: Int32 -> Word32)


data LittleEndian a = LittleEndian { fromLE :: a } deriving (Eq, Show, Functor)

instance Num a => Num (LittleEndian a) where
    (LittleEndian a) + (LittleEndian b) = LittleEndian $ a + b
    (LittleEndian a) * (LittleEndian b) = LittleEndian $ a * b
    (LittleEndian a) - (LittleEndian b) = LittleEndian $ a - b
    abs (LittleEndian a) = LittleEndian $ abs a
    signum (LittleEndian a) = LittleEndian $ signum a
    fromInteger = LittleEndian . fromInteger

instance Binary (LittleEndian Word16) where
    put = putWord16le . fromLE
    get = liftM LittleEndian getWord16le

instance Binary (LittleEndian Word32) where
    put = putWord32le . fromLE
    get = liftM LittleEndian getWord32le

data TGAHeader = TGAHeader {
      _tga_id_field_len     :: Word8
    , _tga_has_color_map    :: Bool
    , _tga_image_type       :: TGAImageType
    , _tga_color_map_offset :: LittleEndian Word16
    , _tga_color_map_length :: LittleEndian Word16
    , _tga_color_map_depth  :: Word8
    , _tga_x_origin         :: LittleEndian Word16
    , _tga_y_origin         :: LittleEndian Word16
    , _tga_width            :: LittleEndian Word16
    , _tga_height           :: LittleEndian Word16
    , _tga_bit_depth        :: Word8
    , _tga_image_descriptor :: TGAImageDescriptor
} deriving (Show, Eq, Generic)

simple_tga_header :: (Integral a) => a -> a -> TGAHeader
-- Usually, it'll be an unindexed, uncompressed, 24-bit image being outputted
simple_tga_header width height = TGAHeader {
      _tga_id_field_len = 0
    , _tga_has_color_map = False
    , _tga_image_type = TGAUncompressedRGBA
    , _tga_color_map_offset = 0
    , _tga_color_map_length = 0
    , _tga_color_map_depth  = 0
    , _tga_x_origin = 0
    , _tga_y_origin = 0
    , _tga_width = fromIntegral width
    , _tga_height = fromIntegral height
    , _tga_bit_depth = 24
    , _tga_image_descriptor = TGAImageDescriptor { _tga_attribute_bits_per_pixel = 0, _tga_screen_origin = TGALowerLeft, _tga_data_storage = TGANonInterleaved }
}

data TGAImageType = TGAUncompressedIndexed | TGAUncompressedRGBA | TGARLEIndexed | TGA_RLE_RGBA deriving (Show, Eq)

instance Enum TGAImageType where
    fromEnum TGAUncompressedIndexed = 1
    fromEnum TGAUncompressedRGBA = 2
    fromEnum TGARLEIndexed = 9
    fromEnum TGA_RLE_RGBA = 10

    toEnum 1 = TGAUncompressedIndexed
    toEnum 2 = TGAUncompressedRGBA
    toEnum 9 = TGARLEIndexed
    toEnum 10 = TGA_RLE_RGBA

instance Binary TGAImageType where
    put = putWord8 . (fromIntegral :: Int -> Word8) . fromEnum
    get = liftM (toEnum . (fromIntegral :: Word8 -> Int)) getWord8

data TGAOrigin = TGALowerLeft | TGAUpperLeft deriving (Show, Eq, Enum, Generic)

data TGADataStorage = TGANonInterleaved
                    | TGAEvenOddInterleaved
                    | TGAFourWayInterleaved
                    | TGAReservedStorageFlag
                    deriving (Show, Eq, Enum, Generic)

data TGAImageDescriptor = TGAImageDescriptor {
      _tga_attribute_bits_per_pixel :: Word8
    , _tga_screen_origin            :: TGAOrigin
    , _tga_data_storage             :: TGADataStorage
} deriving (Show, Eq)

instance Binary TGADataStorage
instance Binary TGAOrigin

instance Binary TGAImageDescriptor where
    get = do
        byte <- getWord8

        let bits_per_pixel = byte .&. 0xF -- 0xF = 0000 1111
        let origin = toEnum . fromIntegral . flip shiftR 5 $ byte .&. 0x20 -- 0x20 = 0010 0000
        let interleaving = toEnum . fromIntegral . flip shiftR 6 $ byte .&. 0xC0 -- 0xC0 = 1100 0000

        return $ TGAImageDescriptor bits_per_pixel origin interleaving
    put desc = do
        let byte = (_tga_attribute_bits_per_pixel desc)
                 + (flip shiftL 5 . fromIntegral . fromEnum $ _tga_screen_origin desc)
                 + (flip shiftL 6 . fromIntegral . fromEnum $ _tga_data_storage desc)

        putWord8 byte

instance Binary TGAHeader

data TGAImage = TGAImage {
      _tga_header     :: TGAHeader
    , _tga_color_map  :: TGAColorMap
    , _tga_image_data :: TGAImageData
} deriving (Show, Eq)

read_tga :: FilePath -> IO TGAImage
read_tga = decodeFile

write_tga :: FilePath -> TGAImage -> IO ()
write_tga = encodeFile


type TGAColorMap = V.Vector RGBColor
type TGAColorMapIndex = Word8 -- At least for the files I'm looking at

data TGAImageData = TGAIndexedData [TGAColorMapIndex] | TGAUnmappedData [RGBColor] deriving (Show, Eq)

-- THE REAL DARK SOULS BEGINS HERE
instance Binary TGAImage where
    get = do
        _tga_header' <- get

        color_map <- liftM V.fromList $ if _tga_has_color_map _tga_header'
                            then do
                                skip $ fromIntegral . fromLE . _tga_color_map_offset $ _tga_header'
                                replicateM (fromIntegral . fromLE . _tga_color_map_length $ _tga_header') $ get_color $ _tga_color_map_depth $ _tga_header'
                            else return []

        image_data <- if _tga_has_color_map _tga_header'
                            then liftM TGAIndexedData $ replicateM (fromIntegral . fromLE $ _tga_width _tga_header' * _tga_height _tga_header') $ (get :: Get TGAColorMapIndex)
                            else liftM TGAUnmappedData $ replicateM (fromIntegral . fromLE $ _tga_width _tga_header' * _tga_height _tga_header') $ get_color $ _tga_bit_depth _tga_header'

        return $ TGAImage _tga_header' color_map image_data
        where
            -- Don't ask me, I'm just a girl!
            five_bit_to_eight_bit five = (five * 527 + 23) `shiftR` 6

            get_color :: Word8 -> Get RGBColor
            get_color 15 = do
                byte1 <- getWord8
                byte2 <- getWord8

                let blue' = byte1 .&. 0x1F
                let green' = ((byte2 `shiftL` 3) .&. 0x1C) .|. ((byte1 `shiftR` 5) .&. 0x07)
                let red' = (byte2 `shiftR` 2) .&. 0x1F

                let alpha = 255
                let red = five_bit_to_eight_bit red'
                let green = five_bit_to_eight_bit green'
                let blue = five_bit_to_eight_bit blue'

                return $ RGBColor red green blue alpha
            get_color 16 = do
                byte1 <- getWord8
                byte2 <- getWord8

                let blue' = byte1 .&. 0x1F
                let green' = ((byte2 `shiftL` 3) .&. 0x1C) .|. ((byte1 `shiftR` 5) .&. 0x07)
                let red' = (byte2 `shiftR` 2) .&. 0x1F

                let alpha = 255 * (byte2 .&. 0x80) `shiftR` 7
                let red = five_bit_to_eight_bit red'
                let green = five_bit_to_eight_bit green'
                let blue = five_bit_to_eight_bit blue'

                return $ RGBColor red green blue alpha
            get_color 24 = do
                blue <- getWord8
                green <- getWord8
                red <- getWord8
                return $ RGBColor red green blue 255
            get_color 32 = do
                blue <- getWord8
                green <- getWord8
                red <- getWord8
                alpha <- getWord8
                return $ RGBColor red green blue alpha
            get_color n = fail "Unsupported pixel format."

    put image = do
        put $ _tga_header image
        replicateM_ (fromIntegral . fromLE . _tga_color_map_offset . _tga_header $ image) $ putWord8 0

        if _tga_has_color_map . _tga_header $ image then
            mapM_ (put_color $ _tga_bit_depth . _tga_header $ image) $ _tga_color_map image
        else
            return ()

        put_image_data (_tga_bit_depth . _tga_header $ image) $ _tga_image_data image
        where
            -- 'what the fuck?' is both appropriate and a good reference here!
            eight_bit_to_five_bit :: Word8 -> Word16 -- Bullshit to make the types line up right, don't question it.
            eight_bit_to_five_bit eight = ((fromIntegral eight :: Word16) * 249 + 1014) `shiftR` 11

            put_color :: Word8 -> RGBColor -> Put
            put_color 15 color = do
                let red' = eight_bit_to_five_bit . _red $ color
                let green' = eight_bit_to_five_bit . _green $ color
                let blue' = eight_bit_to_five_bit . _blue $ color

                let output = red' + (green' `shiftL` 5) + (blue' `shiftL` 10)
                putWord16le output
            put_color 16 color = do
                let red' = eight_bit_to_five_bit . _red $ color
                let green' = eight_bit_to_five_bit . _green $ color
                let blue' = eight_bit_to_five_bit . _blue $ color
                let alpha' = if _alpha color >= 128 then 1 else 0

                let output = red' + (green' `shiftL` 5) + (blue' `shiftL` 10) + (alpha' `shiftL` 15)
                putWord16le output
            put_color 24 color = do
                putWord8 . _blue $ color
                putWord8 . _green $ color
                putWord8 . _red $ color
            put_color 32 color = do
                putWord8 . _blue $ color
                putWord8 . _green $ color
                putWord8 . _red $ color
                putWord8 . _alpha $ color
            put_color n _ = fail "Unsupported pixel format."

            put_image_data :: Word8 -> TGAImageData -> Put
            put_image_data _ (TGAIndexedData indexes) = mapM_ putWord8 indexes
            put_image_data depth (TGAUnmappedData colors) = mapM_ (put_color depth) colors

makeLenses ''TGAHeader
makeLenses ''TGAImageDescriptor
makeLenses ''TGAImage
