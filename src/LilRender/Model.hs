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

import LilRender.Model.Internal
import LilRender.Model.Wavefront

import LilRender.Math.Geometry

data ModelFormat = WavefrontOBJ

loadModel :: ModelFormat -> FilePath -> IO Model
loadModel WavefrontOBJ = loadWavefrontObj

faceToTriangle :: Face -> Triangle (ModelSpace (Point3 Double))
faceToTriangle (Face
                (Vertex (VertexPoint p1) _ _)
                (Vertex (VertexPoint p2) _ _)
                (Vertex (VertexPoint p3) _ _)
               ) = Triangle p1 p2 p3
