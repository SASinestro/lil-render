module LilRender.Model.Internal (
      Vertex(..)
    , Face(..)
    , Model(..)
    ) where

import qualified Data.Vector   as V
import           LilRender.Math.Geometry
import           LilRender.Math.Vector

data Vertex = Vertex {
                _point             :: Point3 Double
              , _textureCoordinate :: Maybe (Point2 Double)
              , _vertexNormal      :: Maybe (Vector3 Double)
        } deriving (Eq, Show)

data Face = Face {
        _firstVertex, _secondVertex, _thirdVertex :: !Vertex
} deriving (Eq, Show)

newtype Model = Model {
      _faces :: V.Vector Face
} deriving (Eq, Show)
