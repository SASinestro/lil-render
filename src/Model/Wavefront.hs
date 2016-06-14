module Model.Wavefront (loadWavefrontObj) where

import Control.Applicative
import Data.Either
import Control.Monad
import Data.Attoparsec.Text
import qualified Data.Text as T

import Model

data UnresolvedFace = UnresolvedFace [(Int, Maybe Int, Maybe Int)] deriving (Eq, Show)

vertex :: Parser Vertex
vertex = do
    char 'v'
    skipSpace
    x <- double
    skipSpace
    y <- double
    skipSpace
    z <- double
    skipWhile $ not . isEndOfLine
    return $ Vector3 x y z

textureCoord :: Parser TextureCoordinate
textureCoord = do
    string "vt"
    skipSpace
    x <- double
    skipSpace
    y <- double
    skipSpace
    z <- double
    skipWhile $ not . isEndOfLine
    return $ Vector3 x y z

vertexNormal :: Parser VertexNormal
vertexNormal = do
    string "vn"
    skipSpace
    x <- double
    skipSpace
    y <- double
    skipSpace
    z <- double
    skipWhile $ not . isEndOfLine
    return $ Vector3 x y z

face :: Parser UnresolvedFace
face = do
    char 'f'
    skipSpace
    recs <- (vertOnly <|> vertText <|> vertNormal <|> vertTextNormal) `sepBy` space
    skipWhile $ not . isEndOfLine
    return $ UnresolvedFace recs

    where
        vertOnly = do
            vert <- signed decimal
            space
            return (vert, Nothing, Nothing)
        vertText = do
            vert <- signed decimal
            char '/'
            text <- signed decimal
            space
            return (vert, Just text, Nothing)
        vertNormal = do
            vert <- signed decimal
            string "//"
            norm <- signed decimal
            return (vert, Nothing, Just norm)
        vertTextNormal = do
            vert <- signed decimal
            char '/'
            text <- signed decimal
            char '/'
            norm <- signed decimal
            return (vert, Just text, Just norm)

vertices :: [T.Text] -> [Vertex]
vertices = rights . map (parseOnly vertex) . filter (("v " ==) . T.take 2)

textureCoords :: [T.Text] -> [TextureCoordinate]
textureCoords = rights . map (parseOnly textureCoord) . filter (("vt" ==) . T.take 2)

vertexNormals :: [T.Text] -> [VertexNormal]
vertexNormals = rights . map (parseOnly vertexNormal) . filter (("vn" ==) . T.take 2)

unresolvedFaces :: [T.Text] -> [UnresolvedFace]
unresolvedFaces = rights . map (parseOnly face) . filter (("f " ==) . T.take 2)

faces :: [Vertex] -> [TextureCoordinate] -> [VertexNormal] -> [UnresolvedFace] -> [Face]
faces vert text norm = fmap (\(UnresolvedFace xs) -> foldr (\(v, t, n) l -> FaceItem (vert !! v) ((text !!) <$> t) ((norm !!) <$> n) : l) [] xs)

loadWavefrontObj :: FilePath -> IO Model
loadWavefrontObj path = do
    obj <- readFile path
    let ls = T.lines . T.pack $ obj

    let v = vertices ls
    let t = textureCoords ls
    let n = vertexNormals ls
    let u = unresolvedFaces ls

    return . Model $ faces v t n u