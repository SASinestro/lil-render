module Math.Vector (Vector2(..), Vector3(..), VectorMath, addVect, subVect, dotVect, crossVect, scaleVect, v2_x, v2_y, v3_x, v3_y, v3_z, magnitudeVect, normalizeVect) where

import Control.Lens
import Data.Function
import Data.Monoid

data Vector2 a = Vector2 {
      _v2_x :: a
    , _v2_y :: a
} deriving (Show, Eq, Functor, Foldable)

data Vector3 a = Vector3 {
      _v3_x :: a
    , _v3_y :: a
    , _v3_z :: a
} deriving (Show, Eq, Functor, Foldable)

makeLenses ''Vector2
makeLenses ''Vector3

instance (Num a) => Monoid (Vector2 a) where
    mempty = zeroVect
    mappend = addVect

instance (Num a) => Monoid (Vector3 a) where
    mempty = zeroVect
    mappend = addVect


class VectorMath v a where
    zeroVect  :: v a
    addVect   :: v a -> v a -> v a
    subVect   :: v a -> v a -> v a
    dotVect   :: v a -> v a -> a
    scaleVect :: v a -> a   -> v a

instance (Num a) => VectorMath Vector2 a where
    zeroVect = Vector2 0 0
    addVect (Vector2 a1 a2) (Vector2 b1 b2) = Vector2 (a1 + b1) (a2 + b2)
    subVect (Vector2 a1 a2) (Vector2 b1 b2) = Vector2 (a1 - b1) (a2 - b2)
    dotVect (Vector2 a1 a2) (Vector2 b1 b2) = a1 * b1 + a2 * b2
    scaleVect vect scale = (* scale) <$> vect

instance (Num a) => VectorMath Vector3 a where
    zeroVect = Vector3 0 0 0
    addVect (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = Vector3 (a1 + a2) (a2 + b2) (a3 + b3)
    subVect (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = Vector3 (a1 - b1) (a2 - b2) (a3 - b3)
    dotVect (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = a1 * b1 + a2 * b2 + a3 * b3
    scaleVect vect scale = (* scale) <$> vect

magnitudeVect :: (VectorMath v a, Floating a, Foldable v) => v a -> a
magnitudeVect = sqrt . foldr (\a b -> a ** 2 + b) 0

normalizeVect :: (VectorMath v a, Floating a, Foldable v) => v a -> v a
normalizeVect vect = vect `scaleVect` (1 / magnitudeVect vect)

crossVect :: Num a => Vector3 a -> Vector3 a -> Vector3 a
crossVect (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = Vector3 (a2 * b3 - a3 * b2) (a3 * b1 - a1 * b3) (a1 * b2 - a2 * b1)
