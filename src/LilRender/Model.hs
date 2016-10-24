module LilRender.Model (
      VertexPoint(..)
    , TextureCoordinate(..)
    , VertexNormal(..)
    , Vertex(..)
    , Face(..)
    , faceToTriangle
    , Model
    , ModelFormat(..)
    , loadModel) where

import qualified Data.Text.IO              as T
import           LilRender.Math.Geometry
import           LilRender.Model.Internal
import           LilRender.Model.Wavefront

#ifdef DEBUG
import           Debug.Trace
import qualified Data.Vector as V
#endif

data ModelFormat = WavefrontOBJ

#ifndef DEBUG
loadModel :: ModelFormat -> FilePath -> IO Model
loadModel WavefrontOBJ path = do
    obj <- T.readFile path
    return $ loadWavefrontObj obj
#else
loadModel :: ModelFormat -> FilePath -> IO Model
loadModel WavefrontOBJ path = do
    obj <- T.readFile path
    let model @ (Model faces) = loadWavefrontObj obj
    traceM $ "Loaded model with " ++ show (V.length faces) ++ " tris."
    return model
#endif

{-# INLINE faceToTriangle #-}
faceToTriangle :: Face -> Triangle (ModelSpace (Point3 Double))
faceToTriangle (Face
                (Vertex (VertexPoint p1) _ _)
                (Vertex (VertexPoint p2) _ _)
                (Vertex (VertexPoint p3) _ _)
               ) = Triangle p1 p2 p3
