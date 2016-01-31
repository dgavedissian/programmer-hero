{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
module Geometry where
import Control.Applicative
import Control.Lens (view)
import Data.Foldable (fold, foldMap)
import Data.Vinyl
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU
import qualified Graphics.VinylGL as VGL
import Graphics.Rendering.OpenGL (($=), GLfloat, GLint, GLsizei)
import Linear

type Pos    = '("vertexPos", V3 GLfloat)
type Normal = '("vertexNormal", V3 GLfloat)
type Colour  = '("vertexColour", V3 GLfloat)

pos :: SField Pos
pos = SField

normal :: SField Normal
normal = SField

col :: SField Colour
col = SField

mvp :: SField '("modelViewProj", M44 GLfloat)
mvp = SField

-- The corners of a square
square :: [V2 GLfloat]
square = V2 <$> [-1,1] <*> [1,-1]

------------------------
-- Board
------------------------

boardVertices :: [FieldRec '[Pos, Normal, Colour]]
boardVertices = map (\p -> pos =: p <+> normal =: z <+> col =: (V3 0.8 0.8 0.8)) positions
    where
        [_,_,z] = basis
        positions = map (\(V2 x z) -> V3 (x * 8) 0 (z * 40)) square

boardIndices :: [GLU.Word32]
boardIndices = [0, 1, 2, 2, 1, 3]

buildBoard :: IO (M44 GLfloat -> IO ())
buildBoard = do
    s <- GLU.simpleShaderProgram "shaders/board.vert" "shaders/board.frag"
    vb <- VGL.bufferVertices boardVertices
    eb <- GLU.makeBuffer GL.ElementArrayBuffer boardIndices
    vao <- GLU.makeVAO $ do
        GL.currentProgram $= Just (GLU.program s)
        VGL.enableVertices' s vb
        VGL.bindVertices vb
        GL.bindBuffer GL.ElementArrayBuffer $= Just eb
    return $ \viewProjMatrix -> GLU.withVAO vao $ do
        GL.currentProgram $= Just (GLU.program s)
        VGL.setUniforms s (mvp =: viewProjMatrix)
        GLU.drawIndexedTris 2

markerVertices :: [FieldRec '[Pos]]
markerVertices = map (\p -> pos =: p) positions
    where
        positions = map (\(V2 x z) -> V3 (x * 2) 0 (z * 2)) square

markerIndices :: [GLU.Word32]
markerIndices = [0, 1, 2, 2, 1, 3]

buildMarker :: IO (M44 GLfloat -> M44 GLfloat -> V3 GLfloat -> IO ())
buildMarker = do
    s <- GLU.simpleShaderProgram "shaders/marker.vert" "shaders/marker.frag"
    vb <- VGL.bufferVertices markerVertices
    eb <- GLU.makeBuffer GL.ElementArrayBuffer markerIndices
    vao <- GLU.makeVAO $ do
        GL.currentProgram $= Just (GLU.program s)
        VGL.enableVertices' s vb
        VGL.bindVertices vb
        GL.bindBuffer GL.ElementArrayBuffer $= Just eb
    return $ \viewProjMatrix modelMatrix c -> GLU.withVAO vao $ do
        GL.currentProgram $= Just (GLU.program s)
        VGL.setUniforms s (mvp =: (viewProjMatrix !*! modelMatrix) <+> colour =: c)
        GLU.drawIndexedTris 2
    where
        colour :: SField '("colour", V3 GLfloat)
        colour = SField


------------------------
-- Notes
------------------------
front,back,left,right,top,bottom :: [V3 GLfloat]
front  = map (\(V2 x y) -> V3 x y 1) square
back   = map (\(V2 x y) -> V3 (-x) y (-1)) square
left   = map (\(V2 z y) -> V3 (-1) y z) square
right  = map (\(V2 z y) -> V3 1 y (-z)) square
top    = map (\(V2 x z) -> V3 x 1 (-z)) square
bottom = map (\(V2 x z) -> V3 x (-1) z) square

noteVerties :: [FieldRec '[Pos]]
noteVerties = map (\p -> pos =: p) $ concat [front, back, left, right, top, bottom]

noteIndices :: [GLU.Word32]
noteIndices = take 36 $ foldMap (flip map faceInds . (+)) [0,4..]
    where
        faceInds = [0, 1, 2, 2, 1, 3]

buildNote :: IO (M44 GLfloat -> M44 GLfloat -> V3 GLfloat -> IO ())
buildNote = do
    s <- GLU.simpleShaderProgram "shaders/note.vert" "shaders/note.frag"
    vb <- VGL.bufferVertices noteVerties
    eb <- GLU.makeBuffer GL.ElementArrayBuffer noteIndices
    vao <- GLU.makeVAO $ do
        GL.currentProgram $= Just (GLU.program s)
        VGL.enableVertices' s vb
        VGL.bindVertices vb
        GL.bindBuffer GL.ElementArrayBuffer $= Just eb
    return $ \viewProjMatrix modelMatrix c -> GLU.withVAO vao $ do
        GL.currentProgram $= Just (GLU.program s)
        VGL.setUniforms s (mvp =: (viewProjMatrix !*! modelMatrix) <+> colour =: c)
        GLU.drawIndexedTris 12
    where
        colour :: SField '("colour", V3 GLfloat)
        colour = SField
