module LilRender.Math.Geometry where

import Control.DeepSeq
import Data.Ix
import Data.Vector.Unboxed          (Unbox)
import Data.Vector.Unboxed.Deriving
import GHC.Generics

import LilRender.Math.Vector

class Point p

data Point2 a = Point2 {
      _p2_x :: !a
    , _p2_y :: !a
} deriving (Show, Eq, Functor, Foldable, Ord, Generic)
instance (Fractional a) => Point (Point2 a)
instance (NFData a) => NFData (Point2 a)

derivingUnbox "Point2"
    [t| forall a. (Unbox a) => Point2 a -> (a, a) |]
    [| \(Point2 x y) -> (x, y) |]
    [| \(x, y) -> (Point2 x y) |]

instance (Ix a) => Ix (Point2 a) where
    range (Point2 p1x p1y, Point2 p2x p2y) = [Point2 i1 i2 | i1 <- range (p1x, p2x), i2 <- range (p1y, p2y)]
    index (Point2 p1x p1y, Point2 p2x p2y) (Point2 px py) = index (p1x, p2x) px * rangeSize (p1y, p2y) + index (p1y, p2y) py
    inRange (Point2 p1x p1y, Point2 p2x p2y) (Point2 px py) = inRange (p1x, p2x) px && inRange (p1y, p2y) py

data Point3 a = Point3 {
      _p3_x :: !a
    , _p3_y :: !a
    , _p3_z :: !a
} deriving (Show, Eq, Functor, Foldable, Ord, Generic)
instance (Fractional a) => Point (Point3 a)
instance (NFData a) => NFData (Point3 a)

derivingUnbox "Point3"
    [t| forall a. (Unbox a) => Point3 a -> (a, a, a) |]
    [| \(Point3 x y z) -> (x, y, z) |]
    [| \(x, y, z) -> (Point3 x y z) |]

instance (Ix a) => Ix (Point3 a) where
    range   (Point3 p1x p1y p1z, Point3 p2x p2y p2z) = [Point3 i1 i2 i3 | i1 <- range (p1x, p2x), i2 <- range (p1y, p2y), i3 <- range (p1z, p2z)]
    index   (Point3 p1x p1y p1z, Point3 p2x p2y p2z) (Point3 px py pz) = index (p1z, p2z) pz + rangeSize (p1z, p2z) * (index (p1y, p2y) py + rangeSize (p1y, p2y) * index (p1x, p2x) px)
    inRange (Point3 p1x p1y p1z, Point3 p2x p2y p2z) (Point3 px py pz) = inRange (p1x, p2x) px && inRange (p1y, p2y) py && inRange (p1z, p2z) pz

newtype ModelSpace a = ModelSpace { fromModelSpace :: a } deriving (Eq, Show, Functor, Ord, Ix, NFData)
newtype World a = World { fromWorld :: a } deriving (Eq, Show, Functor, Ord, Ix, NFData)
newtype Camera a = Camera { fromCamera :: a } deriving (Eq, Show, Functor, Ord, Ix, NFData)
newtype Clip a = Clip { fromClip :: a } deriving (Eq, Show, Functor, Ord, Ix, NFData)
newtype Screen a = Screen { fromScreen :: a } deriving (Eq, Show, Functor, Ord, Ix, NFData)

newtype Barycentric a = Barycentric { fromBarycentric :: a } deriving (Eq, Show, Functor, Ord, Ix, NFData)

derivingUnbox "ModelSpace"
    [t| forall a. (Unbox a) => ModelSpace a -> a |]
    [| \(ModelSpace a) -> a |]
    [| \a -> (ModelSpace a) |]

derivingUnbox "World"
    [t| forall a. (Unbox a) => World a -> a |]
    [| \(World a) -> a |]
    [| \a -> (World a) |]

derivingUnbox "Camera"
    [t| forall a. (Unbox a) => Camera a -> a |]
    [| \(Camera a) -> a |]
    [| \a -> (Camera a) |]

derivingUnbox "Clip"
    [t| forall a. (Unbox a) => Clip a -> a |]
    [| \(Clip a) -> a |]
    [| \a -> (Clip a) |]

derivingUnbox "Screen"
    [t| forall a. (Unbox a) => Screen a -> a |]
    [| \(Screen a) -> a |]
    [| \a -> (Screen a) |]

data Triangle a = Triangle {
        _vertex1, _vertex2, _vertex3 :: !a
} deriving (Show, Eq, Functor, Generic)

instance (NFData a) => NFData (Triangle a)

toBarycentric :: (Real a, Fractional b, Ord b) => Triangle (Screen (Point3 a)) -> Screen (Point2 a) -> Barycentric (Point3 b)
toBarycentric (Triangle (Screen (Point3 p1x p1y _)) (Screen (Point3 p2x p2y _)) (Screen (Point3 p3x p3y _))) (Screen (Point2 px py))
    | abs c < 1 = Barycentric $ Point3 (-1) 1 1
    | otherwise = Barycentric $ Point3 (1 - (a + b)/c) (b/c) (a/c)
    where
        vect1 = Vector3 (p3x - p1x) (p2x - p1x) (p1x - px)
        vect2 = Vector3 (p3y - p1y) (p2y - p1y) (p1y - py)
        (Vector3 a b c) = realToFrac <$> crossVect vect1 vect2

triangularInterpolate :: Double -> Double -> Double -> Barycentric (Point3 Double) -> Double
triangularInterpolate a b c (Barycentric (Point3 lambda1 lambda2 lambda3)) = a * lambda1 + b * lambda2 + c * lambda3
