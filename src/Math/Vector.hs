module Math.Vector (Vector2(..), Vector3(..), crossVect, Vector4, VectorMath, v2_x, v2_y, v3_x, v3_y, v3_z) where

import Control.Lens

data Vector2 a = Vector2 {
      _v2_x :: a
    , _v2_y :: a
} deriving (Show, Eq, Functor)

data Vector3 a = Vector3 {
      _v3_x :: a
    , _v3_y :: a
    , _v3_z :: a
} deriving (Show, Eq, Functor)

crossVect :: Num a => Vector3 a -> Vector3 a -> Vector3 a
crossVect (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = Vector3 (a2 * b3 - a3 * b2) (a3 * b1 - a1 * b3) (a1 * b2 - a2 * b1)

data Vector4 a = Vector4 {
      _v4_x :: a
    , _v4_y :: a
    , _v4_z :: a
    , _v4_w :: a
} deriving (Show, Eq, Functor)

makeLenses ''Vector2
makeLenses ''Vector3

class VectorMath v a where
    addVect   :: v a -> v a -> v a
    subVect   :: v a -> v a -> v a
    dotVect   :: v a -> v a -> a
    scaleVect :: v a -> a   -> v a

instance (Num a) => VectorMath Vector2 a where
    addVect (Vector2 a1 a2) (Vector2 b1 b2) = Vector2 (a1 + b1) (a2 + b2)
    subVect (Vector2 a1 a2) (Vector2 b1 b2) = Vector2 (a1 - b1) (a2 - b2)
    dotVect (Vector2 a1 a2) (Vector2 b1 b2) = a1 * b1 + a2 * b2
    scaleVect vect scale = (* scale) <$> vect

instance (Num a) => VectorMath Vector3 a where
    addVect (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = Vector3 (a1 + a2) (a2 + b2) (a3 + b3)
    subVect (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = Vector3 (a1 - b1) (a2 - b2) (a3 - b3)
    dotVect (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = a1 * b1 + a2 * b2 + a3 * b3
    scaleVect vect scale = (* scale) <$> vect

instance (Num a) => VectorMath Vector4 a where
    addVect (Vector4 a1 a2 a3 a4) (Vector4 b1 b2 b3 b4) = Vector4 (a1 + b1) (a2 + b2) (a3 + b3) (a4 + b4)
    subVect (Vector4 a1 a2 a3 a4) (Vector4 b1 b2 b3 b4) = Vector4 (a1 - b1) (a2 - b2) (a3 - b3) (a4 - b4)
    dotVect (Vector4 a1 a2 a3 a4) (Vector4 b1 b2 b3 b4) = a1 * b1 + a2 * b2 + a3 * b3 + a4 * b4
    scaleVect vect scale = (* scale) <$> vect