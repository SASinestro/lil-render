module Model (Vertex, TextureCoordinate, VertexNormal, FaceItem(..), Face(..), Model(..), vertex, textureCoordinate, vertexNormal, fstFace, sndFace, trdFace, faces) where

import Control.Lens
import Math.Vector

type Vertex = Vector3 Double
type TextureCoordinate = Vector2 Double
type VertexNormal = Vector3 Double

data FaceItem = FaceItem {
        _vertex            :: Vertex
      , _textureCoordinate :: Maybe TextureCoordinate
      , _vertexNormal      :: Maybe VertexNormal
} deriving (Eq, Show)

data Face = Face {
    _fstFace, _sndFace, _trdFace :: FaceItem
} deriving (Show, Eq)

data Model = Model {
      _faces :: [Face]
} deriving (Eq, Show)


makeLenses ''FaceItem
makeLenses ''Face
makeLenses ''Model
