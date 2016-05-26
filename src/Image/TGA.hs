module Image.TGA (
      LittleEndian(..)
    , TGAHeader(..)
    , TGAImageType(..)
    , TGAOrigin(..)
    , TGADataStorage(..)
    , TGAImageDescriptor(..)
    , TGAImage(..)
    , TGAColor(..)
    , TGAImageData
) where

import Control.Monad
import Data.Bits
import Data.Int
import Data.Word
import GHC.Generics

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put


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
      tga_id_field_len     :: Word8
    , tga_color_map_type   :: Word8
    , tga_image_type       :: TGAImageType
    , tga_color_map_offset :: LittleEndian Word16
    , tga_color_map_length :: LittleEndian Word16
    , tga_color_map_depth  :: Word8
    , tga_x_origin         :: LittleEndian Word16
    , tga_y_origin         :: LittleEndian Word16
    , tga_width            :: LittleEndian Word16
    , tga_height           :: LittleEndian Word16
    , tga_bit_depth        :: Word8
    , tga_image_descriptor :: TGAImageDescriptor
} deriving (Show, Eq, Generic)

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
      tga_attribute_bits_per_pixel :: Word8
    , tga_screen_origin :: TGAOrigin
    , tga_data_storage :: TGADataStorage
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
        let byte = (tga_attribute_bits_per_pixel desc)
                 + (flip shiftL 5 . fromIntegral . fromEnum $ tga_screen_origin desc)
                 + (flip shiftL 6 . fromIntegral . fromEnum $ tga_data_storage desc)

        putWord8 byte

instance Binary TGAHeader

data TGAImage = TGAImage {
      tga_header     :: TGAHeader
    , tga_color_map  :: TGAColorMap
    , tga_image_data :: TGAImageData
} deriving (Show, Eq)

data TGAColor = TGAColor {
      tga_red   :: Word8
    , tga_green :: Word8
    , tga_blue  :: Word8
    , tga_alpha :: Word8
} deriving (Show, Eq)

type TGAColorMap = [TGAColor]
type TGAColorMapIndex = Word8 -- At least for the files I'm looking at

data TGAImageData = TGAIndexedData [TGAColorMapIndex] | TGAUnmappedData [TGAColor] deriving (Show, Eq)

-- THE REAL DARK SOULS BEGINS HERE
instance Binary TGAImage where
    get = do
        tga_header' <- get

        color_map <- if tga_color_map_type tga_header' == 1
                            then do
                                skip $ fromIntegral . fromLE . tga_color_map_offset $ tga_header'
                                replicateM (fromIntegral . fromLE . tga_color_map_length $ tga_header') $ get_color $ tga_color_map_depth $ tga_header'
                            else return []

        image_data <- if tga_color_map_type tga_header' == 1
                            then liftM TGAIndexedData $ replicateM (fromIntegral . fromLE $ tga_width tga_header' * tga_height tga_header') $ (get :: Get TGAColorMapIndex)
                            else liftM TGAUnmappedData $ replicateM (fromIntegral . fromLE $ tga_width tga_header' * tga_height tga_header') $ get_color $ tga_bit_depth tga_header'

        return $ TGAImage tga_header' color_map image_data
        where
            -- Don't ask me, I'm just a girl!
            five_bit_to_eight_bit five = (five * 527 + 23) `shiftR` 6

            get_color :: Word8 -> Get TGAColor
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

                return $ TGAColor red green blue alpha
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

                return $ TGAColor red green blue alpha
            get_color 24 = do
                blue <- getWord8
                green <- getWord8
                red <- getWord8
                return $ TGAColor red green blue 255
            get_color 32 = do
                blue <- getWord8
                green <- getWord8
                red <- getWord8
                alpha <- getWord8
                return $ TGAColor red green blue alpha
            get_color n = fail "Unsupported pixel format."

    put image = do
        put $ tga_header image
        replicateM_ (fromIntegral . fromLE . tga_color_map_offset . tga_header $ image) $ putWord8 0
        mapM_ (put_color $ tga_bit_depth . tga_header $ image) $ tga_color_map image
        put_image_data (tga_bit_depth . tga_header $ image) $ tga_image_data image
        where
            -- 'what the fuck?' is both appropriate and a good reference here!
            eight_bit_to_five_bit :: Word8 -> Word16 -- Bullshit to make the types line up right, don't question it.
            eight_bit_to_five_bit eight = ((fromIntegral eight :: Word16) * 249 + 1014) `shiftR` 11

            put_color :: Word8 -> TGAColor -> Put
            put_color 15 color = do
                let red' = eight_bit_to_five_bit . tga_red $ color
                let green' = eight_bit_to_five_bit . tga_green $ color
                let blue' = eight_bit_to_five_bit . tga_blue $ color

                let output = red' + (green' `shiftL` 5) + (blue' `shiftL` 10)
                putWord16le output
            put_color 16 color = do
                let red' = eight_bit_to_five_bit . tga_red $ color
                let green' = eight_bit_to_five_bit . tga_green $ color
                let blue' = eight_bit_to_five_bit . tga_blue $ color
                let alpha' = if tga_alpha color >= 128 then 1 else 0

                let output = red' + (green' `shiftL` 5) + (blue' `shiftL` 10) + (alpha' `shiftL` 15)
                putWord16le output
            put_color 24 color = do
                putWord8 . tga_blue $ color
                putWord8 . tga_green $ color
                putWord8 . tga_red $ color
            put_color 32 color = do
                putWord8 . tga_blue $ color
                putWord8 . tga_green $ color
                putWord8 . tga_red $ color
                putWord8 . tga_alpha $ color

            put_image_data :: Word8 -> TGAImageData -> Put
            put_image_data _ (TGAIndexedData indexes) = mapM_ putWord8 indexes
            put_image_data depth (TGAUnmappedData colors) = mapM_ (put_color depth) colors

