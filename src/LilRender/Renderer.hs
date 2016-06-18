module LilRender.Renderer (drawTexturedModel) where

import Control.Monad
import Control.Monad.Primitive

import LilRender.Image.DrawingPrimitives
import LilRender.Image.Mutable
import LilRender.Math.Geometry
import LilRender.Math.Transform
import LilRender.Model.Internal
import LilRender.Shader
import LilRender.Texture

drawTexturedModel :: forall m shader. (PrimMonad m, Shader shader) =>
                    MutableImage (PrimState m) -- Target
                    -> Model -- The model to be rendered
                    -> Texture -- The texture
                    -> shader (PrimState m) -- The shader
                    -> Transform (ModelSpace (Point3 Double)) (Screen (Point3 Double)) -- The projection from ModelSpace to Screen
                    -> m ()
drawTexturedModel image (Model faces) texture shader projection =
    forM_ faces (\face -> do
        tri <- screenTriangleForFace face
        color <- fragmentShader shader texture
        drawFilledTriangle image color tri
    )
    where
        screenTriangleForFace :: Face -> m (Triangle (Screen (Point3 Double)))
        screenTriangleForFace (Face v1 v2 v3) = do
            (Vertex (VertexPoint p1) _ _) <- vertexShader shader v1 0
            (Vertex (VertexPoint p2) _ _) <- vertexShader shader v2 1
            (Vertex (VertexPoint p3) _ _) <- vertexShader shader v3 2

            let p1' = transform projection p1
            let p2' = transform projection p2
            let p3' = transform projection p3

            return $ Triangle p1' p2' p3'
