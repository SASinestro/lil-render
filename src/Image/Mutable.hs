module Image.Mutable (MutableImage(..), ZBufferIndexType, thawImage, freezeImage, drawPixel) where

import           Data.Vector             ((!))
import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as MV

import           Control.Lens
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST

import           Image
import           Image.Color
import           Math.Geometry

type ZBufferIndexType = Screen (Point3 Int)

data MutableImage s = MutableImage {
      _mStorage :: V.MVector s (RGBColor, Int)
    , _mWidth   :: Int
    , _mHeight  :: Int
    }

thawImage :: PrimMonad m => Image -> m (MutableImage (PrimState m))
thawImage (Image storage width height) = do
    mStorage <- V.thaw . V.map (\c -> (c, 0)) $ storage
    return $ MutableImage mStorage width height

freezeImage :: PrimMonad m => MutableImage (PrimState m) -> m Image
freezeImage (MutableImage mStorage width height) = do
    storage <- V.freeze mStorage
    return $ Image (V.map fst storage) width height

drawPixel :: PrimMonad m => MutableImage (PrimState m) -> ZBufferIndexType -> RGBColor -> m ()
drawPixel (MutableImage mStorage width _) (Screen (Point3 x y z)) color = do
    (_, oldZ) <- MV.read mStorage idx
    when (oldZ < z) $ MV.write mStorage idx (color, z)
        where
            idx = width * y + x
