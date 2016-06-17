module Image (Image(..), ImageIndexType, (<!>), ImageFormat(..), readImage, writeImage, makeImage, ImageConvertible) where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Data.Ix
import           Data.Vector     ((!))
import qualified Data.Vector     as V
import           GHC.Generics    (Generic)

import           Image.Color
import           Image.TGA
import           Math.Geometry
import           Math.Vector

type ImageIndexType = Screen (Point2 Double)

data Image = Image {
      _storage :: V.Vector RGBColor
    , _width   :: Int
    , _height  :: Int
} deriving (Eq, Generic)

instance NFData Image

(<!>) :: Image -> Screen (Point2 Double) -> RGBColor
Image { _storage = storage,  _width = width } <!> (Screen (Point2 x y)) = storage ! (width * round y + round x)

instance Show Image where
    show img@(Image _ w h) = foldl (++) header $ line <$> grid (fromIntegral w) (fromIntegral h)
        where
            grid :: Double -> Double -> [[ ImageIndexType ]]
            grid x y = [ [ Screen $ Point2 i j | i <- [0 .. x - 1] ] |  j <- [0 .. y - 1] ]

            header = "Image (" ++ show w ++ "x" ++ show h ++ ")\n"

            line :: [ ImageIndexType ] -> String
            line (idx:idxs) = foldl (\accum x -> accum ++ " " ++ show (img <!> x)) ("    " ++ show (img <!> idx)) idxs ++ "\n"

class ImageConvertible a where
    toImage :: a -> Image
    fromImage :: Image -> a

data ImageFormat = TGA

readImage :: ImageFormat -> FilePath -> IO Image
readImage TGA path = liftM toImage $ read_tga path

writeImage :: ImageFormat -> FilePath -> Image -> IO ()
writeImage TGA path img = write_tga path $ fromImage img

makeImage :: Int -> Int -> RGBColor -> Image
makeImage width height color = Image {
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
        , _tga_image_data = TGAUnmappedData $ V.toList (_storage image)
    }
