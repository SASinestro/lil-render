module Math.Matrix where

import           Control.Arrow
import           Control.Lens
import           Data.Ix
import           Data.Vector         ((!), (//))
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

import           Math.Vector

data Matrix a = Matrix {
          _mStorage :: V.Vector a
        , _mCols    :: Int
        , _mRows    :: Int
    }

makeLenses ''Matrix

mIndex :: Matrix a -> (Int, Int) -> a
mat `mIndex` (x', y') = _mStorage mat ! (c * y + x)
    where
        c = _mCols mat
        r = _mRows mat
        x = x' - 1
        y = y' - 1

mUpdate :: Matrix a -> [((Int, Int), a)] -> Matrix a
mUpdate mat changes = Matrix (_mStorage mat // fmap (first idx) changes) (_mCols mat) (_mRows mat)
    where
        idx (x, y) = _mCols mat * (y - 1) + (x - 1)

deriving instance (Eq a) => Eq (Matrix a)
deriving instance Functor Matrix

instance (Show a) => Show (Matrix a) where
    show mat = foldl (++) header $ line <$> grid w h
        where
            grid :: Int -> Int -> [[ (Int, Int) ]]
            grid x y = fmap range [ ( (1, j), (x, j) ) | j <- [1 .. y]]

            w = _mCols mat
            h = _mRows mat

            header = "Matrix (" ++ show w ++ "x" ++ show h ++ ")\n"

            line :: [ (Int, Int) ] -> String
            line (idx:idxs) = foldl (\accum x -> accum ++ " " ++ show (mat `mIndex` x)) ("    | " ++ show (mat `mIndex` idx)) idxs ++ " |\n"

matrixFrom2DList :: [[a]] -> Matrix a
matrixFrom2DList lol = Matrix (V.fromList $ concat lol) width height
    where
        width  = length $ head lol
        height = length lol

identityMatrix :: (Num a) => Int -> Matrix a
identityMatrix n = matrixFrom2DList [replicate (j - 1) 0 ++ [1] ++ replicate (n - j) 0 | j <- [1 .. n]]

multiplyMat :: (Num a) => Matrix a -> Matrix a -> Matrix a
multiplyMat a b
    | ay /= bx = error $ "Matrix conformality error: trying to multiply a " ++ show ax ++ "x" ++ show ay
                                                       ++ " matrix with a " ++ show bx ++ "x" ++ show by ++ " matrix."
    | otherwise = Matrix (V.fromList [sum [a `mIndex` (i, k) * (b `mIndex` (k, j)) | k <- [1 .. ay]]
                                                                                | i <- [1 .. ax],
                                                                                  j <- [1 .. by]]) ax by
    where
        ax = a ^. mCols
        ay = a ^. mRows
        bx = b ^. mCols
        by = b ^. mRows
