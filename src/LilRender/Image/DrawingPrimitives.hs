module LilRender.Image.DrawingPrimitives (
      drawFilledTriangle
    ) where

import Control.Monad.Primitive
import qualified Data.Vector.Storable.Mutable as MV
import Linear
import Linear.Affine
import           Foreign.ForeignPtr
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable

import           LilRender.Color
import           LilRender.Image.Mutable
import           LilRender.Math.Geometry

foreign import ccall safe "src/LilRender/Image/DrawingPrimitives.h drawTri" drawTri :: Ptr RGBColor
                                                                                    -> Ptr Int
                                                                                    -> Int 
                                                                                    -> FunPtr (Ptr (Point V3 Double) -> Ptr RGBColor -> IO ()) 
                                                                                    -> Ptr (Point V3 Double)
                                                                                    -> Ptr (Point V3 Double)
                                                                                    -> Ptr (Point V3 Double)
                                                                                    -> IO ()

foreign import ccall "wrapper" wrapColorGetter ::            (Ptr (Point V3 Double) -> Ptr RGBColor -> IO ()) 
                                               -> IO (FunPtr (Ptr (Point V3 Double) -> Ptr RGBColor -> IO ()))

{-# INLINE drawFilledTriangle #-}
drawFilledTriangle :: MutableImage (PrimState IO) RGBColor -> (Point V3 Double -> RGBColor) -> Triangle (Point V3 Double) -> IO ()
drawFilledTriangle (MutableImage pixels zBuffer width _) getColor (Triangle vertex1 vertex2 vertex3) =
    withForeignPtr (fst $ MV.unsafeToForeignPtr0 pixels) (\pixBuf ->
            withForeignPtr (fst $ MV.unsafeToForeignPtr0 zBuffer) (\zBuf -> do
                colorLookup <- wrapGetColor getColor
                vtx1 <- new . fmap realToFrac $ vertex1
                vtx2 <- new . fmap realToFrac $ vertex2
                vtx3 <- new . fmap realToFrac $ vertex3
                drawTri (castPtr pixBuf) zBuf width colorLookup vtx1 vtx2 vtx3
                freeHaskellFunPtr colorLookup
                )
            )

{-# INLINE wrapGetColor #-}
wrapGetColor :: (Point V3 Double -> RGBColor) -> IO (FunPtr (Ptr (Point V3 Double) -> Ptr RGBColor -> IO ()))
wrapGetColor getColor = wrapColorGetter (\ptr outptr -> do
    point <- peek ptr
    poke outptr $ getColor point
    )
