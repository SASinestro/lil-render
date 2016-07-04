module LilRender.Math.Matrix (
      Matrix(..)
    , mIndex
    , mUpdate
    , mMult
    , mTranspose
    , identityMatrix
)where

import           Control.Arrow   (first)
import           Control.DeepSeq
import qualified Data.Ix         as Ix
import           Data.Vector.Storable     ((!), (//))
import qualified Data.Vector.Storable     as V
import           GHC.Generics

import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Ptr
import System.IO.Unsafe

data Matrix a = Matrix {
          _mStorage :: V.Vector a
        , _mCols    :: Int
        , _mRows    :: Int
    }

deriving instance (Eq a, V.Storable a) => Eq (Matrix a)
deriving instance (Generic a) => Generic (Matrix a)
instance (NFData a, Generic a) => NFData (Matrix a)

instance (Show a, V.Storable a) => Show (Matrix a) where
    show mat@(Matrix _ w h) = foldl (++) header $ line <$> grid w h
        where
            grid :: Int -> Int -> [[ (Int, Int) ]]
            grid x y = fmap Ix.range [ ( (1, j), (x, j) ) | j <- [1 .. y]]

            header = "Matrix (" ++ show w ++ "x" ++ show h ++ ")\n"

            line :: [ (Int, Int) ] -> String
            line (idx:idxs) = foldl (\accum x -> accum ++ " " ++ show (mat `mIndex` x)) ("    | " ++ show (mat `mIndex` idx)) idxs ++ " |\n"

{-# INLINE mIndex #-}
mIndex :: (V.Storable a) => Matrix a -> (Int, Int) -> a
(Matrix storage cols _) `mIndex` (x', y') = storage ! (cols * y + x)
    where
        x = x' - 1
        y = y' - 1

{-# INLINE mUpdate #-}
mUpdate :: (V.Storable a) => Matrix a -> [((Int, Int), a)] -> Matrix a
mUpdate (Matrix storage cols rows) changes = Matrix (storage // fmap (first idx) changes) cols rows
    where
        {-# INLINE idx #-}
        idx (x, y) = cols * (y - 1) + (x - 1)

foreign import ccall unsafe "src/LilRender/Math/matrix.h matmult4x4" matmult4x4 :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()

{-# NOINLINE mMult #-}
mMult :: Matrix Double -> Matrix Double -> Matrix Double
mMult a@(Matrix as ax ay) b@(Matrix bs bx by)
    | ay /= bx = error $ "Matrix conformality error: trying to multiply a " ++ show ax ++ "x" ++ show ay
                                                       ++ " matrix with a " ++ show bx ++ "x" ++ show by ++ " matrix."
    | ax == 4 && ay == 4 &&
      bx == 4 && by == 4 =
            unsafePerformIO $ withForeignPtr (fst . V.unsafeToForeignPtr0 $ realToFrac `V.map` as) (\asPtr ->
                withForeignPtr (fst . V.unsafeToForeignPtr0 $ realToFrac `V.map` bs) (\bsPtr -> do
                    outPtr <- mallocArray 16
                    matmult4x4 asPtr bsPtr outPtr
                    outFPtr <- newForeignPtr finalizerFree outPtr
                    return $ Matrix (realToFrac `V.map` V.unsafeFromForeignPtr0 outFPtr 16) ax by
                    )
                )
    | otherwise = Matrix (V.fromList [sum [a `mIndex` (i, k) * (b `mIndex` (k, j)) | k <- [1 .. ay]]
                                                                                   | j <- [1 .. by],
                                                                                     i <- [1 .. ax]]) ax by

mTranspose :: (V.Storable a) => Matrix a -> Matrix a
mTranspose mat@(Matrix _ cols rows) = Matrix (V.fromList [mat `mIndex` (i, j) | i <- [1 .. cols], j <- [1 .. rows] ]) rows cols

--

-- matrixFrom2DList :: [[a]] -> Matrix a
-- matrixFrom2DList lol = Matrix (V.fromList $ concat lol) width height
--     where
--         width  = length $ head lol
--         height = length lol

identityMatrix :: (Num a, V.Storable a) => Int -> Matrix a
identityMatrix 4 = Matrix (V.fromList [1, 0, 0, 0,
                                       0, 1, 0, 0,
                                       0, 0, 1, 0,
                                       0, 0, 0, 1 ]) 4 4
-- identityMatrix n = matrixFrom2DList [replicate (j - 1) 0 ++ [1] ++ replicate (n - j) 0 | j <- [1 .. n]]
