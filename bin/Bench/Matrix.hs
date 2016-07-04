module Bench.Matrix (matrix) where

import           Control.Monad
import           Criterion
import qualified Data.Vector.Storable      as V
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

import           LilRender.Math.Matrix

arbitrary2x2 = liftM3 Matrix (V.fromList <$> vector 4 :: Gen (V.Vector Double)) (return 2) (return 2)

benchMMult2x2 = env (generate $ vectorOf 2 arbitrary2x2) $ bench "Multiply matrices (2x2)" . whnf (\(mat1:mat2:_) -> mMult mat1 mat2)

arbitrary4x4 = liftM3 Matrix (V.fromList <$> vector 16 :: Gen (V.Vector Double)) (return 4) (return 4)

benchMMult4x4 = env (generate $ vectorOf 2 arbitrary4x4) $ bench "Multiply matrices (4x4)" . whnf (\(mat1:mat2:_) -> mMult4x4 mat1 mat2)

arbitrary8x8 = liftM3 Matrix (V.fromList <$> vector 64 :: Gen (V.Vector Double)) (return 8) (return 8)

benchMMult8x8 = env (generate $ vectorOf 2 arbitrary8x8) $ bench "Multiply matrices (8x8)" . whnf (\(mat1:mat2:_) -> mMult mat1 mat2)

matrix = bgroup "Matrices" [
      benchMMult2x2
    , benchMMult4x4
    , benchMMult8x8
    ]
