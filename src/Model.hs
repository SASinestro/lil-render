module Model (
      VertexPoint
    , TextureCoordinate
    , VertexNormal
    , Vertex(..)
    , Face(..)
    , Model(..)
    , point
    , textureCoordinate
    , vertexNormal
    , firstVertex
    , secondVertex
    , thirdVertex
    , vertices
    , faces) where

import Control.Lens
import Math.Vector

type VertexPoint = Vector3 Double
type TextureCoordinate = Vector2 Double
type VertexNormal = Vector3 Double

data Vertex = Vertex {
        _point             :: VertexPoint
      , _textureCoordinate :: Maybe TextureCoordinate
      , _vertexNormal      :: Maybe VertexNormal
} deriving (Eq, Show)

data Face = Face {
        _firstVertex, _secondVertex, _thirdVertex :: Vertex
} deriving (Eq, Show)

vertices :: Fold Face Vertex
vertices = folding (\(Face v1 v2 v3) -> [v1, v2, v3])

data Model = Model {
      _faces :: [Face]
} deriving (Eq, Show)


makeLenses ''Vertex
makeLenses ''Face
makeLenses ''Model
