{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
module Geometry where
import           Data.Vinyl
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU
import qualified Graphics.VinylGL as VGL
import           Graphics.Rendering.OpenGL (($=), GLfloat, GLint, GLsizei)
import           Linear

import qualified Constants as C

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

quadIndices :: [GLU.Word32]
quadIndices = [0, 1, 2, 2, 1, 3]

boardVertices :: [FieldRec '[Pos]]
boardVertices = map (\p -> pos =: p) positions
    where
        positions = map (\(V2 x z) -> V3 (x * (C.boardWidth / 2)) 0 (z * C.boardLength)) square

buildBoard :: IO (M44 GLfloat -> IO ())
buildBoard = do
    s <- GLU.simpleShaderProgram "shaders/board.vert" "shaders/board.frag"
    vb <- VGL.bufferVertices boardVertices
    eb <- GLU.makeBuffer GL.ElementArrayBuffer quadIndices
    vao <- GLU.makeVAO $ do
        GL.currentProgram $= Just (GLU.program s)
        VGL.enableVertices' s vb
        VGL.bindVertices vb
        GL.bindBuffer GL.ElementArrayBuffer $= Just eb
    return $ \viewProjMatrix -> GLU.withVAO vao $ do
        GL.currentProgram $= Just (GLU.program s)
        VGL.setUniforms s (mvp =: viewProjMatrix)
        GLU.drawIndexedTris 2

markerDamageVertices :: [FieldRec '[Pos]]
markerDamageVertices = map (\p -> pos =: p) positions
    where
        positions = map (\(V2 x z) -> V3 (x * (C.boardWidth / 8)) 0 (z * C.boardLength)) square

buildMarkerDamage :: IO (M44 GLfloat -> M44 GLfloat -> V4 GLfloat -> IO ())
buildMarkerDamage = do
    s <- GLU.simpleShaderProgram "shaders/marker-damage.vert" "shaders/marker-damage.frag"
    vb <- VGL.bufferVertices markerDamageVertices
    eb <- GLU.makeBuffer GL.ElementArrayBuffer quadIndices
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
        colour :: SField '("colour", V4 GLfloat)
        colour = SField

markerVertices :: [FieldRec '[Pos]]
markerVertices = map (\p -> pos =: p) positions
    where
        halfSize = C.markerSize / 2
        positions = map (\(V2 x z) -> V3 (x * halfSize) 0 (z * halfSize)) square

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
noteVertices :: [FieldRec '[Pos]]
noteVertices = map (\p -> pos =: p) [
        V3 0 0 (-1),
        V3 1 0 0,
        V3 0 0 1,
        V3 (-1) 0 0,
        V3 0 0.5 0
    ]

noteIndices :: [GLU.Word32]
noteIndices = concatMap (\i -> [i, (i + 3) `mod` 4, 4]) [0..3]

buildNote :: IO (M44 GLfloat -> M44 GLfloat -> V3 GLfloat -> IO ())
buildNote = do
    s <- GLU.simpleShaderProgram "shaders/note.vert" "shaders/note.frag"
    vb <- VGL.bufferVertices noteVertices
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
