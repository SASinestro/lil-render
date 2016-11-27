module LilRender.Image.Mutable (MutableImage(..), drawImageWith, thawImage, freezeImage) where

import           Control.Monad.Primitive
import           Data.STBImage
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV

data MutableImage s a = MutableImage {
      _mPixels  :: V.MVector s a
    , _mZBuffer :: V.MVector s Int
    , _mWidth   :: Int
    , _mHeight  :: Int
    }

drawImageWith :: (PrimMonad m, V.Storable a, Color a) => Int -> Int -> a -> (MutableImage (PrimState m) a -> m ()) -> m (Image a)
drawImageWith _mWidth _mHeight bg creator = do
    _mPixels  <- MV.replicate (_mWidth * _mHeight) bg
    _mZBuffer <- MV.replicate (_mWidth * _mHeight) minBound
    let image = MutableImage{..}
    creator image
    freezeImage image

thawImage :: (PrimMonad m, V.Storable a) => Image a -> m (MutableImage (PrimState m) a)
thawImage Image{..} = do
    _mPixels <- V.thaw _pixels
    _mZBuffer <- MV.replicate (_width * _height) minBound
    let _mWidth  = _width
    let _mHeight = _height
    return MutableImage{..}

freezeImage :: (PrimMonad m, V.Storable a) => MutableImage (PrimState m) a -> m (Image a)
freezeImage MutableImage{..} = do
    _pixels <- V.freeze _mPixels
    let _width = _mWidth 
    let _height = _mHeight
    return Image{..}
