module LilRender.Model.Internal (
      Vertex(..)
    , Model(..)
    ) where

import qualified Data.Vector   as V
import Linear
import Linear.Affine
import           LilRender.Math.Geometry

data Vertex = Vertex {
                _point             :: Point V3 Double
              , _textureCoordinate :: Maybe (Point V2 Double)
              , _vertexNormal      :: Maybe (V3 Double)
        } deriving (Eq, Show)

newtype Model = Model {
      _faces :: V.Vector (Triangle Vertex)
} deriving (Eq, Show)
