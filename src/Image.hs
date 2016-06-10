module Image (Image(..), ImageFormat(..), read_image, write_image, width, height, ImageConvertible) where

import Image.TGA
import Image.Color

import Control.Lens
import Data.Array
import Data.Foldable

type ImageIndexType = (Int, Int)

data Image = Image {
      _storage :: Array ImageIndexType RGBColor
    , _width :: Int
    , _height :: Int
} deriving (Show, Eq)

data ImageFormat = TGA

read_image :: ImageFormat -> FilePath -> IO Image
read_image TGA path = read_tga path

write_image :: ImageFormat -> FilePath -> Image -> IO ()
write_image TGA path img = write_tga path $ fromImage img

makeLenses ''Image

class ImageConvertible a where
    toImage :: a -> Image
    fromImage :: Image -> a

instance ImageConvertible TGAImage where
    toImage tga = Image storage height width
        where
            height = fromIntegral . fromLE $ tga ^. tga_header ^. tga_height
            width  = fromIntegral . fromLE $ tga ^. tga_header ^. tga_width
            from_tga_color tga_color = RGBColor (fromIntegral $ tga_color ^. tga_red)
                                                (fromIntegral $ tga_color ^. tga_green)
                                                (fromIntegral $ tga_color ^. tga_blue)
                                                (fromIntegral (tga_color ^. tga_alpha) / 255)
            storage = listArray ((0, 0), (width - 1, height - 1)) $ case tga ^. tga_image_data of
                (TGAIndexedData indexes) ->
                    let color_map = tga ^. tga_color_map in
                        fmap (\index -> from_tga_color $ color_map ! fromIntegral index) indexes
                (TGAUnmappedData colors) -> fmap from_tga_color colors
    fromImage image = TGAImage {
          _tga_header = simple_tga_header (image ^. width) (image ^. height)
        , _tga_color_map = listArray (0, 1) [TGAColor 0 0 0 0] -- This just gets ignored, and it's easier than an empty array.
        , _tga_image_data = TGAUnmappedData $ fmap to_tga_color (toList $ image ^. storage)
        }
        where
            to_tga_color rgb_color = TGAColor (fromIntegral $ rgb_color ^. red)
                                              (fromIntegral $ rgb_color ^. green)
                                              (fromIntegral $ rgb_color ^. blue)
                                              (round ((rgb_color ^. alpha) * 255))