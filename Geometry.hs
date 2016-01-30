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

-- The corners of a square
square :: [V2 GLfloat]
square = V2 <$> [-1,1] <*> [1,-1]

-- For rendering something, we need a ModelViewProjection matrix
type CamInfo = '[ '("modelViewProj", M44 GLfloat) ]

------------------------
-- Board
------------------------

boardVertices :: [FieldRec [Pos, Normal, Colour]]
boardVertices = map (\p -> pos =: p <+> normal =: z <+> col =: (V3 0.8 0.8
                    0.8)) positions
    where
        [_,_,z] = basis
        positions = map (\(V2 x z) -> V3 x 0 (z * 5)) square

boardIndices :: [GLU.Word32]
boardIndices = [0, 1, 2, 2, 1, 3]

buildBoard :: (CamInfo <: f) => IO (FieldRec f -> IO ())
buildBoard = do
    s <- GLU.simpleShaderProgram "shaders/board.vert" "shaders/board.frag"
    vb <- VGL.bufferVertices boardVertices
    eb <- GLU.makeBuffer GL.ElementArrayBuffer boardIndices
    vao <- GLU.makeVAO $ do
        GL.currentProgram $= Just (GLU.program s)
        VGL.setUniforms s (light =: normalize (V3 0 0 1))
        VGL.enableVertices' s vb
        VGL.bindVertices vb
        GL.bindBuffer GL.ElementArrayBuffer $= Just eb
    return $ \appInfo -> GLU.withVAO vao $ do
        GL.currentProgram $= Just (GLU.program s)
        VGL.setUniforms s (rcast appInfo :: FieldRec CamInfo)
        GLU.drawIndexedTris 12
    where
        light :: SField '("lightDir", V3 GLfloat)
        light = SField

------------------------
-- Cube
------------------------

-- Transform the square into faces
front,back,left,right,top,bottom :: [V3 GLfloat]
front  = map (\(V2 x y) -> V3 x y 1) square
back   = map (\(V2 x y) -> V3 (-x) y (-1)) square
left   = map (\(V2 z y) -> V3 (-1) y z) square
right  = map (\(V2 z y) -> V3 1 y (-z)) square
top    = map (\(V2 x z) -> V3 x 1 (-z)) square
bottom = map (\(V2 x z) -> V3 x (-1) z) square

-- Cube face vertices paired with normal vectors.
cubeVertices :: [FieldRec [Pos,Normal]]
cubeVertices = fold [
        map (setNorm z)    front,
        map (setNorm $ -z) back,
        map (setNorm $ -x) left,
        map (setNorm x)    right,
        map (setNorm y)    top,
        map (setNorm $ -y) bottom
    ]
    where
        [x,y,z] = basis
        setNorm v p = (pos =: p <+> normal =: v)

-- Indices into the vertex array for each face.
cubeIndices :: [GLU.Word32]
cubeIndices = take 36 $ foldMap (flip map faceInds . (+)) [0,4..]
    where
        faceInds = [0, 1, 2, 2, 1, 3]

-- Builds a cube VAO object, and returns a new function which given a
-- record which matches the constraint that it has two matrix fields 'cam'
-- and 'proj', draws that cube object
buildCube :: (CamInfo <: f) => IO (FieldRec f -> IO ())
buildCube = do
    s <- GLU.simpleShaderProgram "shaders/poly.vert" "shaders/poly.frag"
    vb <- VGL.bufferVertices cubeVertices
    eb <- GLU.makeBuffer GL.ElementArrayBuffer cubeIndices
    vao <- GLU.makeVAO $ do
        GL.currentProgram $= Just (GLU.program s)
        VGL.setUniforms s (light =: normalize (V3 0 0 1))
        VGL.enableVertices' s vb
        VGL.bindVertices vb
        GL.bindBuffer GL.ElementArrayBuffer $= Just eb
    return $ \appInfo -> GLU.withVAO vao $ do
        GL.currentProgram $= Just (GLU.program s)
        VGL.setUniforms s (rcast appInfo :: FieldRec CamInfo)
        GLU.drawIndexedTris 12
    where
        light :: SField '("lightDir", V3 GLfloat)
        light = SField
