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
drawFlatShadedModel image model getColor = do
    let vectorsByFace = (_vertex <$>) <$> _faces model

    let v1 = (!! 0)
    let v2 = (!! 1)
    let v3 = (!! 2)

    let normal vs = normalizeVect $ crossVect (v3 vs `subVect` v1 vs) (v2 vs `subVect` v1 vs)

    mapM_ (\vectors -> do
        color <- getColor . normal $ vectors
        drawFilledTriangle image color (v1 vectors, v2 vectors, v3 vectors)) vectorseaByFace

