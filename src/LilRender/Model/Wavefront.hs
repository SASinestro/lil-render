module LilRender.Model.Wavefront (loadWavefrontObj) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Either
import qualified Data.Text                as T
import qualified Data.Vector              as V

import           LilRender.Math.Geometry
import           LilRender.Math.Vector
import           LilRender.Model.Internal

data UnresolvedFace = UnresolvedFace (Int, Maybe Int, Maybe Int) (Int, Maybe Int, Maybe Int) (Int, Maybe Int, Maybe Int) deriving (Eq, Show)

vertexPointParser :: Parser VertexPoint
vertexPointParser = do
    char 'v'
    skipSpace
    x <- double
    skipSpace
    y <- double
    skipSpace
    z <- double
    skipWhile $ not . isEndOfLine
    return . VertexPoint . ModelSpace $ Point3 x y z

textureCoordParser :: Parser TextureCoordinate
textureCoordParser = do
    string "vt"
    skipSpace
    x <- double
    skipSpace
    y <- double
    skipWhile $ not . isEndOfLine
    return . TextureCoordinate $ Point2 x y

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
    return . VertexNormal . ModelSpace $ Vector3 x y z

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

vertices :: [T.Text] -> V.Vector VertexPoint
vertices = V.fromList . rights . fmap (parseOnly vertexPointParser)

textureCoords :: [T.Text] -> V.Vector TextureCoordinate
textureCoords = V.fromList . rights . fmap (parseOnly textureCoordParser)

vertexNormals :: [T.Text] -> V.Vector VertexNormal
vertexNormals = V.fromList . rights . fmap (parseOnly vertexNormalParser)

unresolvedFaces :: [T.Text] -> V.Vector UnresolvedFace
unresolvedFaces = V.fromList . rights . fmap (parseOnly faceParser)

faceResolver vert text norm = fmap faceResolver'
        where
        faceResolver' (UnresolvedFace (v1, t1, n1) (v2, t2, n2) (v3, t3, n3))  = Face (Vertex (vert `lookup` v1) (text `flookup` t1) (norm `flookup` n1))
                                                                                      (Vertex (vert `lookup` v2) (text `flookup` t2) (norm `flookup` n2))
                                                                                      (Vertex (vert `lookup` v3) (text `flookup` t3) (norm `flookup` n3))
        lookup list idx = V.unsafeIndex list (idx - 1)
        flookup list idx = (\index -> V.unsafeIndex list (index - 1)) <$> idx

loadWavefrontObj :: T.Text -> Model
loadWavefrontObj obj = Model $ faceResolver v t n u
    where
        ls = T.lines obj
        v = vertices ls
        t = textureCoords ls
        n = vertexNormals ls
        u = unresolvedFaces ls
