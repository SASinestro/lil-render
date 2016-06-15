module Model.Wavefront where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Either
import qualified Data.Text            as T

import           Math.Vector
import           Model

data UnresolvedFace = UnresolvedFace (Int, Maybe Int, Maybe Int) (Int, Maybe Int, Maybe Int) (Int, Maybe Int, Maybe Int) deriving (Eq, Show)

vertexParser :: Parser Vertex
vertexParser = do
    char 'v'
    skipSpace
    x <- double
    skipSpace
    y <- double
    skipSpace
    z <- double
    skipWhile $ not . isEndOfLine
    return $ Vector3 x y z

textureCoordParser :: Parser TextureCoordinate
textureCoordParser = do
    string "vt"
    skipSpace
    x <- double
    skipSpace
    y <- double
    skipWhile $ not . isEndOfLine
    return $ Vector2 x y

vertexNormalParser :: Parser VertexNormal
vertexNormalParser = do
    string "vn"
    skipSpace
    x <- double
    skipSpace
    y <- double
    skipSpace
    z <- double
    skipWhile $ not . isEndOfLine
    return $ Vector3 x y z

faceParser :: Parser UnresolvedFace
faceParser = do
    char 'f'
    skipSpace
    recs <- (vertTextNormal <|> vertNormal <|> vertText <|> vertOnly) `sepBy` space
    skipWhile $ not . isEndOfLine
    return $ UnresolvedFace (recs !! 0) (recs !! 1) (recs !! 2)

    where
        vertOnly = do
            vert <- signed decimal

            return (vert, Nothing, Nothing)
        vertText = do
            vert <- signed decimal
            char '/'
            text <- signed decimal
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
vertices = rights . map (parseOnly vertexParser) . filter (("v " ==) . T.take 2)

textureCoords :: [T.Text] -> [TextureCoordinate]
textureCoords = rights . map (parseOnly textureCoordParser) . filter (("vt" ==) . T.take 2)

vertexNormals :: [T.Text] -> [VertexNormal]
vertexNormals = rights . map (parseOnly vertexNormalParser) . filter (("vn" ==) . T.take 2)

unresolvedFaces :: [T.Text] -> [UnresolvedFace]
unresolvedFaces = rights . map (parseOnly faceParser) . filter (("f " ==) . T.take 2)

faceResolver vert text norm = fmap faceResolver'
        where
        faceResolver' (UnresolvedFace (v1, t1, n1) (v2, t2, n2) (v3, t3, n3))  = Face (FaceItem (vert `lookup` v1) (text `flookup` t1) (norm `flookup` n1))
                                                                                      (FaceItem (vert `lookup` v2) (text `flookup` t2) (norm `flookup` n2))
                                                                                      (FaceItem (vert `lookup` v3) (text `flookup` t3) (norm `flookup` n3))

        lookup list idx1 = list !! (idx1 - 1)
        flookup list idx1 = (list !!) . (+ (-1)) <$> idx1

loadWavefrontObj :: FilePath -> IO Model
loadWavefrontObj path = do
    obj <- readFile path
    let ls = T.lines . T.pack $ obj

    let v = vertices ls
    let t = textureCoords ls
    let n = vertexNormals ls
    let u = unresolvedFaces ls

    return . Model $ faceResolver v t n u
