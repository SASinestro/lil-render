module LilRender.Math.Matrix (
      Matrix(..)
    , mIndex
    , mUpdate
    , mMult
    , mTranspose
    , identityMatrix
)where

import           Control.Arrow       (first)
import qualified Data.Ix             as Ix
import qualified Data.Vector.Unboxed as V

data Matrix a = Matrix {
          _mStorage :: V.Vector a
        , _mCols    :: Int
        , _mRows    :: Int
    }

deriving instance (Eq a, V.Unbox a) => Eq (Matrix a)

instance (Show a, V.Unbox a) => Show (Matrix a) where
    show mat@(Matrix _ w h) = foldl (++) header $ line <$> grid w h
        where
            grid :: Int -> Int -> [[ (Int, Int) ]]
            grid x y = fmap Ix.range [ ( (1, j), (x, j) ) | j <- [1 .. y]]

            header = "Matrix (" ++ show w ++ "x" ++ show h ++ ")\n"

            line :: [ (Int, Int) ] -> String
            line (idx:idxs) = foldl (\accum x -> accum ++ " " ++ show (mat `mIndex` x)) ("    | " ++ show (mat `mIndex` idx)) idxs ++ " |\n"

{-# INLINE mIndex #-}
mIndex :: (V.Unbox a) => Matrix a -> (Int, Int) -> a
(Matrix storage cols _) `mIndex` (x', y') = storage `V.unsafeIndex` (cols * y + x)
    where
        x = x' - 1
        y = y' - 1

{-# INLINE mUpdate #-}
mUpdate :: (V.Unbox a) => Matrix a -> [((Int, Int), a)] -> Matrix a
mUpdate (Matrix storage cols rows) changes = Matrix (storage `V.unsafeUpd` fmap (first idx) changes) cols rows
    where
        {-# INLINE idx #-}
        idx (x, y) = cols * (y - 1) + (x - 1)

mMult :: (Num a, V.Unbox a) => Matrix a -> Matrix a -> Matrix a
mMult a@(Matrix _ ax ay) b@(Matrix _ bx by)
    | ay /= bx = error $ "Matrix conformality error: trying to multiply a " ++ show ax ++ "x" ++ show ay
                                                       ++ " matrix with a " ++ show bx ++ "x" ++ show by ++ " matrix."
    | otherwise = Matrix (V.fromList [sum [a `mIndex` (i, k) * (b `mIndex` (k, j)) | k <- [1 .. ay]]
                                                                                   | j <- [1 .. by],
                                                                                     i <- [1 .. ax]]) ax by


mTranspose :: (V.Unbox a) => Matrix a -> Matrix a
mTranspose mat@(Matrix _ cols rows) = Matrix (V.fromList [mat `mIndex` (i, j) | i <- [1 .. cols], j <- [1 .. rows] ]) rows cols

--

-- matrixFrom2DList :: [[a]] -> Matrix a
-- matrixFrom2DList lol = Matrix (V.fromList $ concat lol) width height
--     where
--         width  = length $ head lol
--         height = length lol

identityMatrix :: (Num a, V.Unbox a) => Int -> Matrix a
identityMatrix 4 = Matrix (V.fromList [1, 0, 0, 0,
                                       0, 1, 0, 0,
                                       0, 0, 1, 0,
                                       0, 0, 0, 1 ]) 4 4
-- identityMatrix n = matrixFrom2DList [replicate (j - 1) 0 ++ [1] ++ replicate (n - j) 0 | j <- [1 .. n]]
