module Renderer where

import Control.Lens
import Control.Monad
import Control.Monad.Primitive

import Image
import Image.Color
import Image.Drawing.Primitives
import Image.Mutable
import Math.Vector
import Model

drawFlatShadedModel :: (PrimMonad m) => MutableImage (PrimState m) -> Model -> (Vector3 Double -> m RGBColor) -> m ()
drawFlatShadedModel image model getColor =
    mapM_ (\(Face (FaceItem v1 _ _) (FaceItem v2 _ _) (FaceItem v3 _ _)) -> do
        color <- getColor . normalizeVect $ crossVect (v3 `subVect` v1) (v2 `subVect` v1)
        drawFilledTriangle image color (v1, v2, v3)) $ model ^. faces


