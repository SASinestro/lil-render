module LilRender.Image.Immutable (
      Image(..)
    , ImageIndexType
    , (<!>)
    , ImageConvertible
    , toImage
    , fromImage
) where

import           Control.DeepSeq
import           Data.Vector.Unboxed     ((!))
import qualified Data.Vector.Unboxed     as V
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
Image { _storage = storage,  _width = width } <!> (Screen (Point2 x y)) = storage ! (width * y + x)

class ImageConvertible a where
    toImage :: a -> Image
    fromImage :: Image -> a
