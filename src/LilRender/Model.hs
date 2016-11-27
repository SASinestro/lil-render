module LilRender.Model (
      Vertex(..)
    , Face(..)
    , faceToTriangle
    , Model
    , ModelFormat(..)
    , loadModel) where

import qualified Data.Text.IO              as T
import           LilRender.Math.Geometry
import           LilRender.Model.Internal
import           LilRender.Model.Wavefront

data ModelFormat = WavefrontOBJ

loadModel :: ModelFormat -> FilePath -> IO Model
loadModel WavefrontOBJ path = do
    obj <- T.readFile path
    return $ loadWavefrontObj obj

{-# INLINE faceToTriangle #-}
faceToTriangle :: Face -> Triangle (Point3 Double)
faceToTriangle (Face
                (Vertex p1 _ _)
                (Vertex p2 _ _)
                (Vertex p3 _ _)
               ) = Triangle p1 p2 p3
