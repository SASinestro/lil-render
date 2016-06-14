module Model (Vertex, TextureCoordinate, VertexNormal, FaceItem(..), Face, Model(..), vertex, textureCoordinate, vertexNormal, faces) where

import Control.Lens
import Math.Vector

type Vertex = Vector3 Double
type TextureCoordinate = Vector3 Double
type VertexNormal = Vector3 Double

data FaceItem = FaceItem {
        _vertex            :: Vertex
      , _textureCoordinate :: Maybe TextureCoordinate
      , _vertexNormal      :: Maybe VertexNormal
} deriving (Eq, Show)

type Face = [FaceItem]

data Model = Model {
      _faces :: [Face]
} deriving (Eq, Show)


makeLenses ''FaceItem
makeLenses ''Model
