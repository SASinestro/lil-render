module LilRender.Image.Immutable (
      Image(..)
    , ImageIndexType
    , (<!>)
    , ImageConvertible
    , toImage
    , fromImage
    , makeImage
) where

import           Control.DeepSeq
import qualified Data.Vector.Storable    as V
import           GHC.Generics            (Generic)

import           LilRender.Color
import           LilRender.Math.Geometry

data Image = Image {
      _storage :: V.Vector RGBColor
    , _width   :: Int
    , _height  :: Int
} deriving (Eq, Generic)

instance Show Image where
    show (Image _ w h) = "Image (" ++ show w ++ "x" ++ show h ++ ")\n"

instance NFData Image

type ImageIndexType = Screen (Point2 Int)

(<!>) :: Image -> ImageIndexType -> RGBColor
Image { _storage = storage,  _width = width } <!> (Screen (Point2 x y)) = storage `V.unsafeIndex` (width * y + x)

class ImageConvertible a where
    toImage :: a -> Image
    fromImage :: Image -> a


makeImage :: Int -> Int -> RGBColor -> Image
makeImage width height color = Image {
      _storage = V.replicate (width * height) color
    , _width = width
    , _height = height
}
