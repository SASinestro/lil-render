module BMP.Format (
      BMPSize
    , BMPDataOffset
    , BitmapHeader(..)
    , bitmap_header
    , BitmapCompressionType(..)
    , BitDepth(..)
    , DIBHeader(..)
    , ColorTableItem(..)
    , ColorTable(..)
    , PixelData
    , Pixel(..)
    , Bitmap(..)
) where

import           Data.Word
import           Data.Int
import           Data.Char (ord)
import           Data.Bits
import           Control.Monad (liftM, replicateM)

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

type BMPSize = Word32
type BMPDataOffset = Word32

data BitmapHeader = BitmapHeader {
      bmp_header_id        :: (Word8, Word8)
    , bmp_header_img_size  :: BMPSize
    , bmp_header_reserved1 :: Word16
    , bmp_header_reserved2 :: Word16
    , bmp_header_offset    :: BMPDataOffset
} deriving (Show, Eq)

instance Binary BitmapHeader where
    put header = do
        put $ bmp_header_id header
        putWord32le $ bmp_header_img_size header
        putWord16le $ bmp_header_reserved1 header
        putWord16le $ bmp_header_reserved2 header
        putWord32le $ bmp_header_offset header
    get = do
        id        <- get
        size      <- getWord32le
        reserved1 <- getWord16le
        reserved2 <- getWord16le
        offset    <- getWord32le
        return $ BitmapHeader id size reserved1 reserved2 offset


char2ascii :: Char -> Word8
char2ascii = fromIntegral . ord

bitmap_header :: BMPSize -> BMPDataOffset -> BitmapHeader
bitmap_header size offset = BitmapHeader {
      bmp_header_id        =  (char2ascii 'B', char2ascii 'M')
    , bmp_header_img_size      = size
    , bmp_header_reserved1 = 0
    , bmp_header_reserved2 = 0
    , bmp_header_offset    = offset
}


getInt16le = liftM (fromIntegral :: Word16 -> Int16) getWord16le
getInt32le = liftM (fromIntegral :: Word32 -> Int32) getWord32le

putInt16le = putWord16le . (fromIntegral :: Int16 -> Word16)
putInt32le = putWord32le . (fromIntegral :: Int32 -> Word32)


data BitmapCompressionType = Compression_None
                           | Compression_RLE8
                           | Compression_RLE4
                           | Compression_Bitfields
                           deriving (Eq, Show, Enum)

instance Binary BitmapCompressionType where
    put = putWord32le . (fromIntegral :: Int -> Word32) . fromEnum
    get = liftM (toEnum . (fromIntegral :: Word32 -> Int)) getWord32le


data BitDepth = BitsPerPixel_1
              | BitsPerPixel_4
              | BitsPerPixel_8
              | BitsPerPixel_16
              | BitsPerPixel_24
              deriving (Eq, Ord, Show)

instance Enum BitDepth where
    toEnum 1  = BitsPerPixel_1
    toEnum 4  = BitsPerPixel_4
    toEnum 8  = BitsPerPixel_8
    toEnum 16 = BitsPerPixel_16
    toEnum 24 = BitsPerPixel_24

    fromEnum BitsPerPixel_1  = 1
    fromEnum BitsPerPixel_4  = 4
    fromEnum BitsPerPixel_8  = 8
    fromEnum BitsPerPixel_16 = 16
    fromEnum BitsPerPixel_24 = 24

instance Binary BitDepth where
    put = putWord16le . (fromIntegral :: Int -> Word16) . fromEnum
    get = liftM (toEnum . (fromIntegral :: Word16 -> Int)) getWord16le


data DIBHeader = BitmapInfoHeader {
      dib_header_size             :: Word32
    , dib_header_width            :: Int32
    , dib_header_height           :: Int32
    , dib_header_color_planes     :: Word16
    , dib_header_bpp              :: BitDepth
    , dib_header_compression      :: BitmapCompressionType
    , dib_header_img_size         :: Word32
    , dib_header_horiz_res        :: Int32
    , dib_header_vert_res         :: Int32
    , dib_header_num_colors       :: Word32
    , dib_header_important_colors :: Word32
} deriving (Show, Eq)

instance Binary DIBHeader where
    put dib = do
        putWord32le $ dib_header_size dib
        putInt32le $ dib_header_width dib
        putInt32le $ dib_header_height dib
        putWord16le $ dib_header_color_planes dib
        put $ dib_header_bpp dib
        put $ dib_header_compression dib
        putWord32le $ dib_header_img_size dib
        putInt32le $ dib_header_horiz_res dib
        putInt32le $ dib_header_vert_res dib
        putWord32le $ dib_header_num_colors dib
        putWord32le $ dib_header_important_colors dib
    get = do
        size <- getWord32le
        width <- getInt32le
        height <- getInt32le
        color_planes <- getWord16le
        bpp <- get
        compression <- get
        img_size <- getWord32le
        h_res <- getInt32le
        v_res <- getInt32le
        num_colors <- getWord32le
        important_colors <- getWord32le

        return $ BitmapInfoHeader size width height color_planes bpp compression img_size h_res v_res num_colors important_colors


data ColorTableItem = ColorTableItem_RGB {
      color_rgb_red   :: Word8
    , color_rgb_green :: Word8
    , color_rgb_blue  :: Word8
} deriving (Show, Eq)

instance Binary ColorTableItem where
    put color = do
        put $ color_rgb_red color
        put $ color_rgb_green color
        put $ color_rgb_blue color
        putWord8 0
    get = do
        r <- get
        g <- get
        b <- get
        skip 1

        return $ ColorTableItem_RGB r g b

type ColorTable = [ColorTableItem]


class PixelData a

data Pixel a where
    Pixel :: (PixelData a) => a -> Pixel a

deriving instance (Show a) => Show (Pixel a)

instance PixelData Bool -- 1-bit images
instance PixelData Word8 -- 4-bit and 8-bit images
instance PixelData (Word8, Word8, Word8) -- 16-bit and 24-bit images

getPixelRow :: Int32 -> BitDepth -> ColorTable -> Get ([Pixel (Word8, Word8, Word8)])
getPixelRow px BitsPerPixel_1 table = undefined
getPixelRow px BitsPerPixel_4 table = undefined
getPixelRow px BitsPerPixel_8 table = undefined
getPixelRow px' BitsPerPixel_16 _ = do
    pixels <- replicateM px $ getPixel'
    skip $ padding px
    return pixels
    where
        px = fromIntegral px'

        red_mask = 0xF800
        green_mask = 0x7E0
        blue_mask = 0x1F

        getPixel' = do
            pixel <- getWord16le

            let red = shiftR (pixel .&. red_mask) 11
            let green = shiftR (pixel .&. green_mask) 5
            let blue = pixel .&. blue_mask

            return $ Pixel (red, green, blue)

        modulus = px `mod` 2
        padding px = if modulus /= 0 then 2 - modulus else 0
getPixelRow px' BitsPerPixel_24 _ = do
    pixels <- replicateM px $ getPixel'
    skip $ padding px
    return pixels
    where
        px = fromIntegral px'

        getPixel' = do
            red <- getWord8
            green <- getWord8
            blue <- getWord8
            return $ Pixel (red, green, blue)

        modulus = px * 3 `mod` 4
        padding px = if modulus /= 0 then 4 - modulus else 0


data Bitmap = Bitmap {
      bmp_header :: BitmapHeader
    , dib_header :: DIBHeader
    , color_table :: ColorTable
} deriving (Show, Eq)

instance Binary Bitmap where
    put bmp = do
        put $ bmp_header bmp
        put $ dib_header bmp
        sequence_ $ map put $ color_table bmp
    get = do
        bmp_header <- get
        dib_header <- get

        let bit_depth = dib_header_bpp dib_header
        let num_colors = fromIntegral $ dib_header_num_colors dib_header
        color_table <- if bit_depth < BitsPerPixel_16 then
                            replicateM (if num_colors /= 0 then num_colors else (^) 2 $ fromEnum bit_depth) (get :: Get ColorTableItem) else return []

        return $ Bitmap bmp_header dib_header color_table