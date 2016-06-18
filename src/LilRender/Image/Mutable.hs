module LilRender.Image.Mutable (MutableImage(..), ZBufferIndexType, drawImageWith, thawImage, freezeImage, drawPixel) where

import           Control.Monad
import           Control.Monad.Primitive
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

import           LilRender.Color
import           LilRender.Image.Immutable
import           LilRender.Math.Geometry

type ZBufferIndexType = Screen (Point3 Int)

data MutableImage s = MutableImage {
      _mStorage :: V.MVector s RGBColor
    , _mZBuffer :: V.MVector s Int
    , _mWidth   :: Int
    , _mHeight  :: Int
    }

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

drawPixel :: PrimMonad m => MutableImage (PrimState m) -> ZBufferIndexType -> RGBColor -> m ()
drawPixel (MutableImage mStorage zBuffer width _) (Screen (Point3 x y z)) color = do
    oldZ <- MV.read zBuffer idx
    when (oldZ < z) $ do
        MV.write mStorage idx color
        MV.write zBuffer idx z
        where
            idx = width * y + x
