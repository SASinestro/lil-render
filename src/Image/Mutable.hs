module Image.Mutable (MutableImage, (<!!>), thawImage, freezeImage, drawPixel) where

import           Data.Vector             ((!))
import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as MV

import           Control.Lens
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST

import           Image
import           Image.Color

data MutableImage s = MutableImage {
      _mStorage :: V.MVector s RGBColor
    , _mWidth   :: Int
    , _mHeight  :: Int
    }

(<!!>) :: (PrimMonad m) => MutableImage (PrimState m) -> ImageIndexType -> m RGBColor
img <!!> (x, y) = do
    let stor = _mStorage img
    MV.read stor (idx x y)
        where
            w = _mWidth img
            idx x' y' = w * y' + x'

thawImage :: PrimMonad m => Image -> m (MutableImage (PrimState m))
thawImage img = do
    stor <- V.thaw $ _storage img
    return $ MutableImage stor (_width img) (_height img)

freezeImage :: PrimMonad m => MutableImage (PrimState m) -> m Image
freezeImage img = do
    let stor' = _mStorage img
    stor  <- V.freeze stor'
    return $ Image stor (_mWidth img) (_mHeight img)

drawPixel :: PrimMonad m => MutableImage (PrimState m) -> RGBColor -> ImageIndexType -> m ()
drawPixel img color (x, y) = do
    let stor = _mStorage img
    MV.write stor (idx x y) color
        where
            w = _mWidth img
            idx x' y' = w * y'+ x'
