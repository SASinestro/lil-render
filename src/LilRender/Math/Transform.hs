module LilRender.Math.Transform (
      Transform
    , transform
    , transformBy
    , (>>>)

    , identityTransform
    , translationTransform
    , zoomTransform
    , xRotateTransform
    , yRotateTransform
    , zRotateTransform

    , cameraTransform
    , orthographicProjectionTransform
    , viewportTransform
) where

import           Control.DeepSeq
import qualified Data.Vector.Generic     as V
import           GHC.Generics

import           LilRender.Math.Geometry
import           LilRender.Math.Matrix
import           LilRender.Math.Vector

data Transform a b = Transform (Matrix Double) deriving (Show, Eq, Generic)
instance NFData (Transform a b) where rnf !_ = ()

class Transformable a where
    toMatrix :: a -> Matrix Double
    fromMatrix :: Matrix Double -> a

instance Transformable (Point3 Double) where
    toMatrix (Point3 a b c) = Matrix (V.fromList [a, b, c, 1]) 4 1
    fromMatrix mat@(Matrix _ cols rows)
        | cols == 4 && rows == 1 = Point3 (a / d) (b / d) (c / d)
        | otherwise = error "Non 4x1 matrices cannot be converted into 3D coordinates."
            where
                a = mat `mIndex` (1, 1)
                b = mat `mIndex` (2, 1)
                c = mat `mIndex` (3, 1)
                d = mat `mIndex` (4, 1)

instance Transformable (Vector3 Double) where
    toMatrix (Vector3 a b c) = Matrix (V.fromList [a, b, c, 1]) 4 1
    fromMatrix mat@(Matrix _ cols rows)
        | cols == 4 && rows == 1 = Vector3 (a / d) (b / d) (c / d)
        | otherwise = error "Non 4x1 matrices cannot be converted into 3D coordinates."
            where
                a = mat `mIndex` (1, 1)
                b = mat `mIndex` (2, 1)
                c = mat `mIndex` (3, 1)
                d = mat `mIndex` (4, 1)

deriving instance (Transformable a) => Transformable (ModelSpace a)
deriving instance (Transformable a) => Transformable (World a)
deriving instance (Transformable a) => Transformable (Camera a)
deriving instance (Transformable a) => Transformable (Clip a)
deriving instance (Transformable a) => Transformable (Screen a)

transform :: (Transformable a, Transformable b) => Transform a b -> a -> b
transform (Transform mat) a =  fromMatrix (mMult mat (toMatrix a))

transformBy :: (Transformable a, Transformable b) => a -> Transform a b -> b
transformBy = flip transform

infixr 1 >>>
(>>>) :: (Transformable a, Transformable b, Transformable c) => Transform a b -> Transform b c -> Transform a c
(Transform a) >>> (Transform b) =  Transform $ mMult b a

identityTransform :: (Transformable a, Transformable b) => Transform a b
identityTransform = Transform $ identityMatrix 4

translationTransform :: (Transformable a, Transformable b) =>  Double -> Double -> Double -> Transform a b
translationTransform dx dy dz =
    Transform $ identityMatrix 4 `mUpdate` [((1, 4), dx), ((2, 4), dy), ((3, 4), dz)]

zoomTransform :: (Transformable a, Transformable b) =>  Double -> Transform a b
zoomTransform factor = Transform $ identityMatrix 4 `mUpdate` [((i, i), factor) | i <- [1, 3]]

xRotateTransform :: (Transformable a, Transformable b) =>  Double -> Transform a b
xRotateTransform angle =
    Transform $ identityMatrix 4 `mUpdate` [((2, 2), cos angle), ((3, 3), cos angle), ((2, 3), -sin angle), ((3, 2), sin angle)]

yRotateTransform :: (Transformable a, Transformable b) =>  Double -> Transform a b
yRotateTransform angle =
    Transform $ identityMatrix 4 `mUpdate` [((1, 1), cos angle), ((3, 3), cos angle), ((1, 3), sin angle), ((3, 1), -sin angle)]

zRotateTransform :: (Transformable a, Transformable b) => Double -> Transform a b
zRotateTransform angle =
    Transform $ identityMatrix 4 `mUpdate` [((1, 1), cos angle), ((2, 2), cos angle), ((1, 2), -sin angle), ((2, 1), sin angle)]

--

type CameraLocation = World (Point3 Double)
type CameraTarget = World (Point3 Double)

cameraTransform' :: (Transformable a) => CameraLocation -> CameraTarget -> World (Vector3 Double) -> Transform (World a) (Camera a)
cameraTransform' (World (Point3 clx cly clz)) (World (Point3 ctx cty ctz)) (World up)
    = Transform $ mMult minv translate
    where
        z @(Vector3 z1 z2 z3) = normalizeVect $ Vector3 (clx - ctx) (cly - cty) (clz - ctz) -- z is a normal vector from the camera's location to its target
        x @(Vector3 x1 x2 x3) = normalizeVect $ crossVect up z
        (Vector3 y1 y2 y3)    = normalizeVect $ crossVect z x
        minv = Matrix (V.fromList [x1, y1, z1, 0,
                                   x2, y2, z2, 0,
                                   x3, y3, z3, 0,
                                   0,  0,  0,  1]) 4 4
        translate = Matrix (V.fromList [1, 0, 0, -ctx,
                                        0, 1, 0, -cty,
                                        0, 0, 1, -ctz,
                                        0, 0, 0, 1     ]) 4 4

cameraTransform ::CameraLocation -> CameraTarget -> Transform (World (Point3 Double)) (Camera (Point3 Double))
cameraTransform location target = cameraTransform' location target (World (Vector3 0 1 0)) -- Up is usually up.

orthographicProjectionTransform :: (Transformable a) => World (Point3 Double) -> Transform (Camera a) (Clip a)
orthographicProjectionTransform (World p) = Transform $ Matrix (V.fromList [1, 0, 0, 0,
                                                                            0, 1, 0, 0,
                                                                            0, 0, 1, -1/magnitude p,
                                                                            0, 0, 0, 1 ]) 4 4
    where magnitude = sqrt . foldr (\a b -> a ** 2 + b) 0

type CenterPoint = (Screen (Point2 Int))
type Width = Int
type Height = Int
type Depth = Int

viewportTransform' :: CenterPoint -> Width -> Height -> Depth -> Transform (Clip (Point3 Double)) (Screen (Point3 Double))
viewportTransform' (Screen (Point2 x' y')) width height depth =
    Transform $ Matrix (V.fromList [ w / 2,  0,  0,  0,  0,  h / 2,  0,  0,  0,  0,  d / 2,  0,  x + w / 2,  y + h / 2,  d / 2,  1 ]) 4 4
    where
        w = fromIntegral width  :: Double
        h = fromIntegral height :: Double
        d = fromIntegral depth  :: Double

        x = fromIntegral x'
        y = fromIntegral y'

viewportTransform :: CenterPoint -> Width -> Height -> Transform (Clip (Point3 Double)) (Screen (Point3 Double))
viewportTransform center width height = viewportTransform' center width height 255
