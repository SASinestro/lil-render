module LilRender.Math.Geometry (
      Triangle(..)
    , triangularInterpolate
    , triangularInterpolateVector
    , transformVector
    , transformPoint
    ) where

import Linear
import Linear.Affine
import Test.QuickCheck.Arbitrary (Arbitrary(..))

data Triangle a = Triangle {
        _vertex1, _vertex2, _vertex3 :: !a
} deriving (Show, Eq, Functor)

instance (Arbitrary a) => Arbitrary (Triangle a) where
  arbitrary = Triangle <$> arbitrary <*> arbitrary <*> arbitrary

{-# INLINE triangularInterpolate #-}
triangularInterpolate :: Triangle Double -> Point V3 Double -> Double
triangularInterpolate (Triangle a b c) (P (V3 x y z)) = a * x + b * y + c * z

{-# INLINE triangularInterpolateVector #-}
triangularInterpolateVector :: Additive f => Triangle (f Double) -> Point V3 Double -> f Double
triangularInterpolateVector (Triangle a b c) (P (V3 x y z)) = (a ^* x) ^+^ (b ^* y) ^+^ (c ^* z)

transformVector :: M44 Double -> V3 Double -> V3 Double
transformVector mat vect = (\(V4 x y z _) -> V3 x y z) $ mat !* (vector vect)

transformPoint :: M44 Double -> Point V3 Double -> Point V3 Double
transformPoint mat (P vect) = P . normalizePoint $ (point vect) *! mat
