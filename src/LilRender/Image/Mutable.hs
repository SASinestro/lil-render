module LilRender.Image.Mutable (MutableImage(..), ZBufferIndexType, drawImageWith, thawImage, freezeImage) where

import           Control.DeepSeq
import           Control.Monad.Primitive
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           GHC.Generics

import           LilRender.Color
import           LilRender.Image.Immutable
import           LilRender.Math.Geometry

import Debug.Trace

type ZBufferIndexType = Screen (Point3 Int)

data MutableImage s = MutableImage {
      _mStorage :: V.MVector s RGBColor
    , _mZBuffer :: V.MVector s Int
    , _mWidth   :: Int
    , _mHeight  :: Int
    } deriving (Generic)

instance NFData (MutableImage s)

drawImageWith :: (PrimMonad m) => Int -> Int -> RGBColor -> (MutableImage (PrimState m) -> m ()) -> m Image
drawImageWith width height bg creator = do
    storage <- MV.replicate (width * height) bg
    zBuffer <- MV.replicate (width * height) minBound
    let image = MutableImage storage zBuffer width height
    creator image
    freezeImage image

thawImage :: PrimMonad m => Image -> m (MutableImage (PrimState m))
thawImage (Image storage width height) = do
    mStorage <- V.thaw storage
    zBuffer <- MV.replicate (width * height) minBound
    return $ MutableImage mStorage zBuffer width height

freezeImage :: PrimMonad m => MutableImage (PrimState m) -> m Image
freezeImage (MutableImage mStorage _ width height) = do
    storage <- V.freeze mStorage
    return $ Image storage width height
