module LilRender.Model (
      Vertex(..)
    , Model
    , loadWavefrontModel
    ) where

import qualified Data.Text.IO              as T
import           LilRender.Model.Internal
import           LilRender.Model.Wavefront

loadWavefrontModel :: FilePath -> IO Model
loadWavefrontModel path = do
    obj <- T.readFile path
    return $ loadWavefrontObj obj
