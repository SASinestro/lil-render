module LilRender.Model (
      VertexPoint(..)
    , TextureCoordinate(..)
    , VertexNormal(..)
    , Vertex(..)
    , Face(..)
    , Model(..)) where

import qualified Data.Vector             as V
import           LilRender.Math.Geometry
import           LilRender.Math.Vector


newtype VertexPoint = VertexPoint { unwrapVertexPoint :: World (Point3 Double) } deriving (Show, Eq)
newtype TextureCoordinate = TextureCoordinate { unwrapTextureCoordinate :: Point2 Double } deriving (Show, Eq)
newtype VertexNormal = VertexNormal { unwrapVertexNormal :: World (Vector3 Double) } deriving (Show, Eq)

data Vertex = Vertex {
                point             :: VertexPoint
              , textureCoordinate :: Maybe TextureCoordinate
              , vertexNormal      :: Maybe VertexNormal
        } deriving (Eq, Show)

data Face = Face {
        firstVertex, secondVertex, thirdVertex :: !Vertex
} deriving (Eq, Show)

newtype Model = Model {
      faces :: V.Vector Face
} deriving (Eq, Show)
