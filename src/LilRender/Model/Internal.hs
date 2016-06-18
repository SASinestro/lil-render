module LilRender.Model.Internal (
      VertexPoint(..)
    , TextureCoordinate(..)
    , VertexNormal(..)
    , Vertex(..)
    , Face(..)
    , Model(..)
    ) where

import           Control.DeepSeq
import qualified Data.Vector                  as V
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics
import           LilRender.Math.Geometry
import           LilRender.Math.Vector


newtype VertexPoint = VertexPoint { unwrapVertexPoint :: ModelSpace (Point3 Double) } deriving (Show, Eq, NFData)
newtype TextureCoordinate = TextureCoordinate { unwrapTextureCoordinate :: Point2 Double } deriving (Show, Eq, NFData)
newtype VertexNormal = VertexNormal { unwrapVertexNormal :: ModelSpace (Vector3 Double) } deriving (Show, Eq, NFData)

derivingUnbox "VertexPoint"
    [t| VertexPoint -> ModelSpace (Point3 Double) |]
    [| unwrapVertexPoint |]
    [| VertexPoint |]

derivingUnbox "TextureCoordinate"
    [t| TextureCoordinate -> Point2 Double |]
    [| unwrapTextureCoordinate |]
    [| TextureCoordinate |]

derivingUnbox "VertexNormal"
    [t| VertexNormal -> ModelSpace (Vector3 Double) |]
    [| unwrapVertexNormal |]
    [| VertexNormal |]

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
