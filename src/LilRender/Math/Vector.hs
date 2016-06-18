module LilRender.Math.Vector (
      Vector2(..)
    , Vector3(..)
    , VectorMath
    , dotVect
    , crossVect
    , scaleVect
    , magnitudeVect
    , normalizeVect
    ) where

import Data.Vector.Unboxed          (Unbox)
import Data.Vector.Unboxed.Deriving

data Vector2 a = Vector2 {
      v2_x :: !a
    , v2_y :: !a
} deriving (Show, Eq, Functor, Foldable)

data Vector3 a = Vector3 {
      v3_x :: !a
    , v3_y :: !a
    , v3_z :: !a
} deriving (Show, Eq, Functor, Foldable)

derivingUnbox "Vector2"
    [t| forall a. (Unbox a) => Vector2 a -> (a, a) |]
    [| \(Vector2 x y) -> (x, y) |]
    [| \(x, y) -> (Vector2 x y) |]

derivingUnbox "Vector3"
    [t| forall a. (Unbox a) => Vector3 a -> (a, a, a) |]
    [| \(Vector3 x y z) -> (x, y, z) |]
    [| \(x, y, z) -> (Vector3 x y z) |]

instance (Num a) => Monoid (Vector2 a) where
    mempty = zeroVect
    mappend = (+)

instance (Num a) => Monoid (Vector3 a) where
    mempty = zeroVect
    mappend = (+)

instance (Num a) => Num (Vector2 a) where
    (Vector2 a1 a2) + (Vector2 b1 b2) = Vector2 (a1 + b1) (a2 + b2)
    (Vector2 a1 a2) - (Vector2 b1 b2) = Vector2 (a1 - b1) (a2 - b2)
    (Vector2 a1 a2) * (Vector2 b1 b2) = Vector2 (a1 * b1) (a2 * b2)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger i = fromInteger <$> Vector2 i i

instance (Num a) => Num (Vector3 a) where
    (Vector3 a1 a2 a3) + (Vector3 b1 b2 b3) = Vector3 (a1 + a2) (a2 + b2) (a3 + b3)
    (Vector3 a1 a2 a3) - (Vector3 b1 b2 b3) = Vector3 (a1 - b1) (a2 - b2) (a3 - b3)
    (Vector3 a1 a2 a3) * (Vector3 b1 b2 b3) = Vector3 (a1 * b1) (a2 * b2) (a3 * b3)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger i = fromInteger <$> Vector3 i i i

class (Num a) => VectorMath v a where
    zeroVect  :: v a
    dotVect   :: v a -> v a -> a
    scaleVect :: v a -> a   -> v a

instance (Num a) => VectorMath Vector2 a where
    zeroVect = Vector2 0 0
    dotVect (Vector2 a1 a2) (Vector2 b1 b2) = a1 * b1 + a2 * b2
    scaleVect vect scale = (* scale) <$> vect

instance (Num a) => VectorMath Vector3 a where
    zeroVect = Vector3 0 0 0
    dotVect (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = a1 * b1 + a2 * b2 + a3 * b3
    scaleVect vect scale = (* scale) <$> vect

magnitudeVect :: (VectorMath v a, Floating a, Foldable v) => v a -> a
magnitudeVect = sqrt . foldr (\a b -> a ** 2 + b) 0

normalizeVect :: (VectorMath v a, Floating a, Foldable v) => v a -> v a
normalizeVect vect = vect `scaleVect` (1 / magnitudeVect vect)

crossVect :: Num a => Vector3 a -> Vector3 a -> Vector3 a
crossVect (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = Vector3 (a2 * b3 - a3 * b2) (a3 * b1 - a1 * b3) (a1 * b2 - a2 * b1)
