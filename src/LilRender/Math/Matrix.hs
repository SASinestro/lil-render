module LilRender.Math.Matrix (
      Matrix(..)
    , mIndex
    , mUpdate
    , mMult
    , mTranspose

    , matrixFrom2DList
    , identityMatrix
)where

import           Control.Arrow         (first)
import qualified Data.Ix               as Ix
import           Data.Vector           ((!), (//))
import qualified Data.Vector           as V
import qualified Data.Vector.Mutable   as MV

import           LilRender.Math.Vector

data Matrix a = Matrix {
          _mStorage :: V.Vector a
        , _mCols    :: Int
        , _mRows    :: Int
    }

deriving instance (Eq a) => Eq (Matrix a)
deriving instance Functor Matrix

instance (Show a) => Show (Matrix a) where
    show mat@(Matrix _ w h) = foldl (++) header $ line <$> grid w h
        where
            grid :: Int -> Int -> [[ (Int, Int) ]]
            grid x y = fmap Ix.range [ ( (1, j), (x, j) ) | j <- [1 .. y]]

            header = "Matrix (" ++ show w ++ "x" ++ show h ++ ")\n"

            line :: [ (Int, Int) ] -> String
            line (idx:idxs) = foldl (\accum x -> accum ++ " " ++ show (mat `mIndex` x)) ("    | " ++ show (mat `mIndex` idx)) idxs ++ " |\n"

{-# INLINE mIndex #-}
mIndex :: Matrix a -> (Int, Int) -> a
(Matrix storage cols _) `mIndex` (x', y') = storage ! (cols * y + x)
    where
        x = x' - 1
        y = y' - 1

{-# INLINE mUpdate #-}
mUpdate :: Matrix a -> [((Int, Int), a)] -> Matrix a
mUpdate (Matrix storage cols rows) changes = Matrix (storage // fmap (first idx) changes) cols rows
    where
        {-# INLINE idx #-}
        idx (x, y) = cols * (y - 1) + (x - 1)

mMult :: (Num a) => Matrix a -> Matrix a -> Matrix a
mMult a@(Matrix _ ax ay) b@(Matrix _ bx by)
    | ay /= bx = error $ "Matrix conformality error: trying to multiply a " ++ show ax ++ "x" ++ show ay
                                                       ++ " matrix with a " ++ show bx ++ "x" ++ show by ++ " matrix."
    | otherwise = Matrix (V.fromList [sum [a `mIndex` (i, k) * (b `mIndex` (k, j)) | k <- [1 .. ay]]
                                                                                   | j <- [1 .. by],
                                                                                     i <- [1 .. ax]]) ax by


mTranspose :: Matrix a -> Matrix a
mTranspose mat@(Matrix _ cols rows) = Matrix (V.fromList [mat `mIndex` (i, j) | i <- [1 .. cols], j <- [1 .. rows] ]) rows cols

--

matrixFrom2DList :: [[a]] -> Matrix a
matrixFrom2DList lol = Matrix (V.fromList $ concat lol) width height
    where
        width  = length $ head lol
        height = length lol

identityMatrix :: (Num a) => Int -> Matrix a
identityMatrix n = matrixFrom2DList [replicate (j - 1) 0 ++ [1] ++ replicate (n - j) 0 | j <- [1 .. n]]
