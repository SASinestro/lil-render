module LilRender.Image.STB (stbLoadImage, stbWritePNG, stbWriteBMP, stbWriteTGA) where

import LilRender.Image.Immutable

import qualified Data.Vector.Storable    as V

import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "stb/stb_image.h stbi_load" stbi_load :: CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> CInt -> IO (Ptr CUChar)
foreign import ccall "stb/stb_image.h &stbi_image_free" stbi_image_free :: FunPtr (Ptr CUChar -> IO ())

foreign import ccall "stb/stb_image_write.h stbi_write_png" stbi_write_png :: CString -> CInt -> CInt -> CInt -> Ptr CUChar -> CInt -> IO CInt
foreign import ccall "stb/stb_image_write.h stbi_write_bmp" stbi_write_bmp :: CString -> CInt -> CInt -> CInt -> Ptr CUChar -> IO CInt
foreign import ccall "stb/stb_image_write.h stbi_write_tga" stbi_write_tga :: CString -> CInt -> CInt -> CInt -> Ptr CUChar -> IO CInt

stbLoadImage :: FilePath -> IO Image
stbLoadImage path = do
    cPath <- newCString path
    widthPtr <- new 0
    heightPtr <- new 0
    nComponentsPtr <- new 0

    dataPtr <- stbi_load cPath widthPtr heightPtr nComponentsPtr 3 -- 3 bytes per pixel (assumption!)
    dataForeignPtr <- newForeignPtr stbi_image_free dataPtr

    width  <- fromIntegral <$> peek widthPtr :: IO Int
    height <- fromIntegral <$> peek heightPtr :: IO Int

    let storage = V.unsafeFromForeignPtr0 dataForeignPtr (width * height * 3)

    free cPath
    free widthPtr
    free heightPtr
    free nComponentsPtr

    return $ Image (V.unsafeCast storage) width height

stbWritePNG :: FilePath -> Image -> IO ()
stbWritePNG path (Image storage width height) = do
    cPath <- newCString path

    let w = fromIntegral width :: CInt
    let h = fromIntegral height :: CInt

    withForeignPtr (fst $ V.unsafeToForeignPtr0 storage) (\pixBuf ->
        stbi_write_png cPath w h 3 (castPtr pixBuf) (w * 3) -- bytes per row
        )

    free cPath

stbWriteBMP :: FilePath -> Image -> IO ()
stbWriteBMP path (Image storage width height) = do
    cPath <- newCString path

    let w = fromIntegral width :: CInt
    let h = fromIntegral height :: CInt

    withForeignPtr (fst $ V.unsafeToForeignPtr0 storage) $ stbi_write_bmp cPath w h 3 . castPtr

    free cPath

stbWriteTGA :: FilePath -> Image -> IO ()
stbWriteTGA path (Image storage width height) = do
    cPath <- newCString path

    let w = fromIntegral width :: CInt
    let h = fromIntegral height :: CInt

    withForeignPtr (fst $ V.unsafeToForeignPtr0 storage) $ stbi_write_tga cPath w h 3 . castPtr

    free cPath
