module LilRender.Math.Geometry where

import Control.DeepSeq
import Control.Monad
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

data Point2 a = Point2 {
      _p2_x :: !a
    , _p2_y :: !a
} deriving (Show, Eq, Functor, Foldable, Ord)

instance (Storable a) => Storable (Point2 a) where
    sizeOf _ = 2 * sizeOf (undefined :: a)
    alignment _ = alignment (undefined :: a)
    peek ptr = do
        let ptr' = castPtr ptr :: Ptr a
        a <- peekElemOff ptr' 0
        b <- peekElemOff ptr' 1
        return $ Point2 a b
    poke ptr (Point2 a b) = do
        let ptr' = castPtr ptr :: Ptr a
        pokeElemOff ptr' 0 a
        pokeElemOff ptr' 1 b

data Point3 a = Point3 {
      _p3_x :: !a
    , _p3_y :: !a
    , _p3_z :: !a
} deriving (Show, Eq, Functor, Foldable, Ord, Generic)
instance (NFData a) => NFData (Point3 a)

instance (Storable a) => Storable (Point3 a) where
    sizeOf _ = 3 * sizeOf (undefined :: a)
    alignment _ = alignment (undefined :: a)
    peek ptr = do
        let ptr' = castPtr ptr :: Ptr a
        a <- peekElemOff ptr' 0
        b <- peekElemOff ptr' 1
        c <- peekElemOff ptr' 2
        return $ Point3 a b c
    poke ptr (Point3 a b c) = do
        let ptr' = castPtr ptr :: Ptr a
        pokeElemOff ptr' 0 a
        pokeElemOff ptr' 1 b
        pokeElemOff ptr' 2 c

instance Arbitrary (Point3 Double) where
  arbitrary = do
      a <- choose (0, 800)
      b <- choose (0, 800)
      c <- choose (0, 800)
      return $ Point3 a b c

data Triangle a = Triangle {
        _vertex1, _vertex2, _vertex3 :: !a
} deriving (Show, Eq, Functor, Generic)
instance (NFData a) => NFData (Triangle a)

instance (Arbitrary a) => Arbitrary (Triangle a) where
  arbitrary = liftM3 Triangle arbitrary arbitrary arbitrary

{-# INLINE triangularInterpolate #-}
triangularInterpolate :: Double -> Double -> Double -> Point3 Double -> Double
triangularInterpolate a b c (Point3 lambda1 lambda2 lambda3) = a * lambda1 + b * lambda2 + c * lambda3
