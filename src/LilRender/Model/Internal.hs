module LilRender.Model.Internal (
      VertexPoint(..)
    , TextureCoordinate(..)
    , VertexNormal(..)
    , Vertex(..)
    , Face(..)
    , Model(..)
    ) where

import           Control.DeepSeq
import qualified Data.Vector   as V
import           GHC.Generics
import           Foreign.Storable
import           LilRender.Math.Geometry
import           LilRender.Math.Vector


newtype VertexPoint = VertexPoint { unwrapVertexPoint :: ModelSpace (Point3 Double) } deriving (Show, Eq, NFData, Storable)
newtype TextureCoordinate = TextureCoordinate { unwrapTextureCoordinate :: Point2 Double } deriving (Show, Eq, NFData, Storable)
newtype VertexNormal = VertexNormal { unwrapVertexNormal :: ModelSpace (Vector3 Double) } deriving (Show, Eq, NFData, Storable)

data Vertex = Vertex {
                _point             :: VertexPoint
              , _textureCoordinate :: Maybe TextureCoordinate
              , _vertexNormal      :: Maybe VertexNormal
        } deriving (Eq, Show, Generic)
instance NFData Vertex

data Face = Face {
        _firstVertex, _secondVertex, _thirdVertex :: !Vertex
} deriving (Eq, Show, Generic)
instance NFData Face

newtype Model = Model {
      _faces :: V.Vector Face
} deriving (Eq, Show, Generic)
instance NFData Model
