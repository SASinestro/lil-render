module LilRender.Math.Geometry where

import Control.DeepSeq
import Control.Monad
import Data.Ix
import GHC.Generics
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Foreign.Storable
import Foreign.Ptr

data Point2 a = Point2 {
      _p2_x :: !a
    , _p2_y :: !a
} deriving (Show, Eq, Functor, Foldable, Ord, Generic)
instance (NFData a) => NFData (Point2 a)

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

instance (Ix a) => Ix (Point2 a) where
    range (Point2 p1x p1y, Point2 p2x p2y) = [Point2 i1 i2 | i1 <- range (p1x, p2x), i2 <- range (p1y, p2y)]
    index (Point2 p1x p1y, Point2 p2x p2y) (Point2 px py) = index (p1x, p2x) px * rangeSize (p1y, p2y) + index (p1y, p2y) py
    inRange (Point2 p1x p1y, Point2 p2x p2y) (Point2 px py) = inRange (p1x, p2x) px && inRange (p1y, p2y) py

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

instance (Ix a) => Ix (Point3 a) where
    range   (Point3 p1x p1y p1z, Point3 p2x p2y p2z) = [Point3 i1 i2 i3 | i1 <- range (p1x, p2x), i2 <- range (p1y, p2y), i3 <- range (p1z, p2z)]
    index   (Point3 p1x p1y p1z, Point3 p2x p2y p2z) (Point3 px py pz) = index (p1z, p2z) pz + rangeSize (p1z, p2z) * (index (p1y, p2y) py + rangeSize (p1y, p2y) * index (p1x, p2x) px)
    inRange (Point3 p1x p1y p1z, Point3 p2x p2y p2z) (Point3 px py pz) = inRange (p1x, p2x) px && inRange (p1y, p2y) py && inRange (p1z, p2z) pz

instance Arbitrary (Screen (Point3 Double)) where
  arbitrary = do
      a <- choose (0, 800)
      b <- choose (0, 800)
      c <- choose (0, 800)
      return $ Screen (Point3 a b c)

newtype ModelSpace a = ModelSpace { fromModelSpace :: a } deriving (Eq, Show, Functor, Ord, Ix, Storable, NFData)
newtype World a = World { fromWorld :: a } deriving (Eq, Show, Functor, Ord, Ix, Storable, NFData)
newtype Camera a = Camera { fromCamera :: a } deriving (Eq, Show, Functor, Ord, Ix, Storable, NFData)
newtype Clip a = Clip { fromClip :: a } deriving (Eq, Show, Functor, Ord, Ix, Storable, NFData)
newtype Screen a = Screen { fromScreen :: a } deriving (Eq, Show, Functor, Ord, Ix, Storable, NFData)
newtype Barycentric a = Barycentric { fromBarycentric :: a } deriving (Eq, Show, Functor, Ord, Ix, Storable)

data Triangle a = Triangle {
        _vertex1, _vertex2, _vertex3 :: !a
} deriving (Show, Eq, Functor, Generic)
instance (NFData a) => NFData (Triangle a)

instance (Arbitrary a) => Arbitrary (Triangle a) where
  arbitrary = liftM3 Triangle arbitrary arbitrary arbitrary

triangularInterpolate :: Double -> Double -> Double -> Barycentric (Point3 Double) -> Double
triangularInterpolate a b c (Barycentric (Point3 lambda1 lambda2 lambda3)) = a * lambda1 + b * lambda2 + c * lambda3
