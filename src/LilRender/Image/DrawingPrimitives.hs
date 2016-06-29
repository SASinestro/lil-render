module LilRender.Image.DrawingPrimitives (drawFilledTriangle
    , drawTri, wrapColorGetter, wrapGetColor
    ) where

import           Control.Monad.Primitive
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV

import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

import LilRender.Color
import LilRender.Image.Mutable
import LilRender.Math.Geometry

foreign import ccall safe "src/LilRender/Image/DrawingPrimitives.h drawTri" drawTri :: Ptr CChar -> Ptr Int -> Int -> FunPtr (Ptr (Barycentric (Point3 Double)) -> IO (Ptr (Maybe RGBColor))) -> Ptr (Point3 Double) -> Ptr (Point3 Double) -> Ptr (Point3 Double) -> IO ()

foreign import ccall "wrapper" wrapColorGetter :: (Ptr (Barycentric (Point3 Double)) -> IO (Ptr (Maybe RGBColor))) -> IO (FunPtr (Ptr (Barycentric (Point3 Double)) -> IO (Ptr (Maybe RGBColor))))

drawFilledTriangle :: MutableImage (PrimState IO) -> (Barycentric (Point3 Double) -> Maybe RGBColor) -> Triangle (Screen (Point3 Double)) -> IO ()
drawFilledTriangle (MutableImage pixels zBuffer width _) getColor (Triangle (Screen vertex1) (Screen vertex2) (Screen vertex3)) =
    withForeignPtr (fst $ MV.unsafeToForeignPtr0 pixels) (\pixBuf ->
            withForeignPtr (fst $ MV.unsafeToForeignPtr0 zBuffer) (\zBuf -> do
                colorLookup <- wrapGetColor getColor
                vtx1 <- new . fmap realToFrac $ vertex1
                vtx2 <- new . fmap realToFrac $ vertex2
                vtx3 <- new . fmap realToFrac $ vertex3
                drawTri (castPtr pixBuf) zBuf width colorLookup vtx1 vtx2 vtx3
                )
            )

wrapGetColor :: (Barycentric (Point3 Double) -> Maybe RGBColor) -> IO (FunPtr (Ptr (Barycentric (Point3 Double)) -> IO (Ptr (Maybe RGBColor))))
wrapGetColor getColor = wrapColorGetter (\ptr -> do
    point <- peek ptr
    Foreign.Marshal.new $ getColor point
    )
