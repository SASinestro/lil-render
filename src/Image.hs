module Image (Image(..), ImageIndexType, (<!>), storage, width, height, ImageFormat(..), read_image, write_image, make_image, ImageConvertible) where

import Image.TGA
import Image.Color

import Control.Monad
import Control.Lens
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.Foldable
import Data.Ix

type ImageIndexType = (Int, Int)


data Image = Image {
      _storage :: V.Vector RGBColor
    , _width :: Int
    , _height :: Int
} deriving (Eq)

(<!>) :: Image -> ImageIndexType -> RGBColor
img <!> (x, y) = (_storage img) ! (w * y + x)
                where
                    w = _width img
                    h = _height img

instance Show Image where
    show img = foldl (++) header $ fmap line $ grid w h
        where
            grid :: Int -> Int -> [[ ImageIndexType ]]
            grid x y = fmap range [ ( (i, 0), (i, y - 1) ) | i <- [0 .. x - 1]]

            w = _width img
            h = _height img

            header = "Image (" ++ (show w) ++ "x" ++ (show h) ++ ")\n"

            line :: [ ImageIndexType ] -> String
            line (idx:idxs) = (foldl (\accum x -> accum ++ " " ++ (show $ img <!> x)) ("    " ++ (show $ img <!> idx)) idxs) ++ "\n"

makeLenses ''Image

class ImageConvertible a where
    toImage :: a -> Image
    fromImage :: Image -> a


data ImageFormat = TGA

read_image :: ImageFormat -> FilePath -> IO Image
read_image TGA path = liftM toImage $ read_tga path

write_image :: ImageFormat -> FilePath -> Image -> IO ()
write_image TGA path img = write_tga path $ fromImage img

make_image :: Int -> Int -> RGBColor -> Image
make_image width height color = Image {
      _storage = V.replicate (width * height) color
    , _width = width
    , _height = height
}


instance ImageConvertible TGAImage where
    toImage tga = Image storage height width
        where
            height = fromIntegral . fromLE $ tga ^. tga_header . tga_height
            width  = fromIntegral . fromLE $ tga ^. tga_header . tga_width
            color_map = tga ^. tga_color_map
            storage = V.fromList $ case tga ^. tga_image_data of
                (TGAIndexedData indexes) -> fmap (\index -> color_map ! fromIntegral index) indexes
                (TGAUnmappedData colors) -> colors
    fromImage image = TGAImage {
          _tga_header = simple_tga_header (_width image) (_height image)
        , _tga_color_map = V.empty
        , _tga_image_data = TGAUnmappedData $ (V.toList $ _storage image)
    }