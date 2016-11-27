module LilRender.Renderer (drawTexturedModel) where

import Control.Monad
import Control.Monad.Primitive
import LilRender.Color
import LilRender.Image.DrawingPrimitives
import LilRender.Image.Mutable
import LilRender.Math.Geometry
import LilRender.Math.Transform
import LilRender.Model.Internal
import LilRender.Shader
import LilRender.Texture

drawTexturedModel :: (Shader shader) =>
                       Model -- The model to be rendered
                    -> Texture -- The texture
                    -> shader (PrimState IO) -- The shader
                    -> Transform (Point3 Double) (Point3 Double) -- The projection from ModelSpace to Screen
                    -> MutableImage (PrimState IO) RGBColor -- Target
                    -> IO ()
drawTexturedModel (Model faces) texture shader projection image =
    forM_ faces (\face -> do
        tri <- screenTriangleForFace face
        color <- fragmentShader shader texture
        drawFilledTriangle image color tri
    )
    where
        screenTriangleForFace :: Face -> IO (Triangle (Point3 Double))
        screenTriangleForFace (Face v1 v2 v3) = do
            (Vertex p1 _ _) <- vertexShader shader v1 0
            (Vertex p2 _ _) <- vertexShader shader v2 1
            (Vertex p3 _ _) <- vertexShader shader v3 2

            let p1' = transform projection p1
            let p2' = transform projection p2
            let p3' = transform projection p3

            return $ Triangle p1' p2' p3'
