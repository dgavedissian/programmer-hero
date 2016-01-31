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

boardVertices :: [FieldRec [Pos, Normal, Colour]]
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
        VGL.setUniforms s (light =: normalize (V3 0 0 1))
        VGL.enableVertices' s vb
        VGL.bindVertices vb
        GL.bindBuffer GL.ElementArrayBuffer $= Just eb
    return $ \viewProjMatrix -> GLU.withVAO vao $ do
        GL.currentProgram $= Just (GLU.program s)
        VGL.setUniforms s (mvp =: viewProjMatrix)
        GLU.drawIndexedTris 2
    where
        light :: SField '("lightDir", V3 GLfloat)
        light = SField

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

noteVerties :: [FieldRec [Pos,Normal]]
noteVerties = fold [
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

noteIndices :: [GLU.Word32]
noteIndices = take 36 $ foldMap (flip map faceInds . (+)) [0,4..]
    where
        faceInds = [0, 1, 2, 2, 1, 3]

buildNote :: IO (M44 GLfloat -> M44 GLfloat -> IO ())
buildNote = do
    s <- GLU.simpleShaderProgram "shaders/note.vert" "shaders/note.frag"
    vb <- VGL.bufferVertices noteVerties
    eb <- GLU.makeBuffer GL.ElementArrayBuffer noteIndices
    vao <- GLU.makeVAO $ do
        GL.currentProgram $= Just (GLU.program s)
        VGL.setUniforms s (light =: normalize (V3 0 0 1))
        VGL.enableVertices' s vb
        VGL.bindVertices vb
        GL.bindBuffer GL.ElementArrayBuffer $= Just eb
    return $ \viewProjMatrix modelMatrix -> GLU.withVAO vao $ do
        GL.currentProgram $= Just (GLU.program s)
        VGL.setUniforms s (mvp =: (viewProjMatrix !*! modelMatrix))
        GLU.drawIndexedTris 12
    where
        light :: SField '("lightDir", V3 GLfloat)
        light = SField
