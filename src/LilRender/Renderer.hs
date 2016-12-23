module LilRender.Renderer (drawTexturedModel) where

import Control.Monad
import Control.Monad.Primitive
import Linear
import LilRender.Color
import LilRender.Image.DrawingPrimitives
import LilRender.Image.Mutable
import LilRender.Math.Geometry
import LilRender.Model.Internal
import LilRender.Shader

drawTexturedModel :: (Shader shader) =>
                       Model -- The model to be rendered
                    -> shader (PrimState IO) -- The shader
                    -> M44 Double -- The projection from ModelSpace to Screen
                    -> MutableImage (PrimState IO) RGBColor -- Target
                    -> IO ()
drawTexturedModel (Model faces) shader projection image =
    forM_ faces (\face -> do
        tri <- vertexShader shader face
        let screenTri = transformPoint projection <$> tri
        color <- fragmentShader shader
        drawFilledTriangle image color screenTri
    )
