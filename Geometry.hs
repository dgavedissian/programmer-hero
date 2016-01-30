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
import System.FilePath ((</>))

type Pos    = '("vertexPos", V3 GLfloat)
type Normal = '("vertexNormal", V3 GLfloat)
type Color  = '("vertexColor", V3 GLfloat)

pos :: SField Pos
pos = SField

normal :: SField Normal
normal = SField

col :: SField Color
col = SField

-- The 2D corners of a square.
square :: [V2 GLfloat]
square = V2 <$> [-1,1] <*> [1,-1]

-- Transform the square into faces
front,back,left,right,top,bottom :: [V3 GLfloat]
front  = map (\(V2 x y) -> V3 x y 1) square
back   = map (\(V2 x y) -> V3 (-x) y (-1)) square
left   = map (\(V2 z y) -> V3 (-1) y z) square
right  = map (\(V2 z y) -> V3 1 y (-z)) square
top    = map (\(V2 x z) -> V3 x 1 (-z)) square
bottom = map (\(V2 x z) -> V3 x (-1) z) square

-- Cube face vertices paired with normal vectors.
pts :: [FieldRec [Pos,Normal]]
pts = fold [ map (setNorm z)    front
           , map (setNorm $ -z) back
           , map (setNorm $ -x) left
           , map (setNorm x)    right
           , map (setNorm y)    top
           , map (setNorm $ -y) bottom ]
  where [x,y,z] = basis
        setNorm v p = (pos =: p <+> normal =: v)

-- Color the front vertices a dark blue, the back a light beige.
colorize :: FieldRec [Pos,Normal] -> FieldRec [Pos,Normal,Color]
colorize pt = pt <+> col =: c
  where c | view (rlens pos.rfield._z) pt > 0 =
              V3 8.235294e-2 0.20392157 0.3137255
          | otherwise = V3 0.95686275 0.8392157 0.7372549

-- Indices into the vertex array for each face.
inds :: [GLU.Word32]
inds = take 36 $ foldMap (flip map faceInds . (+)) [0,4..]
  where faceInds = [0,1,2,2,1,3]

-- For rendering a cube, we'll need a ModelView matrix, and a
-- ProjectionModelView matrix.
type CamInfo = '[ '("modelView", M44 GLfloat), '("proj", M44 GLfloat) ]

-- Builds a cube VAO object, and returns a new function which given a
-- record which matches the constraint that it has two matrix fields 'cam'
-- and 'proj', draws that cube object
buildCube :: (CamInfo <: f) => IO (FieldRec f -> IO ())
buildCube = do
    s <- GLU.simpleShaderProgram ("shaders"</>"poly.vert") ("shaders"</>"poly.frag")
    vb <- VGL.bufferVertices (map colorize pts)
    eb <- GLU.makeBuffer GL.ElementArrayBuffer inds
    vao <- GLU.makeVAO $
         do GL.currentProgram $= Just (GLU.program s)
            -- Take a Vinyl record, and bind the uniforms
            VGL.setUniforms s (light =: normalize (V3 0 0 1))
            VGL.enableVertices' s vb
            VGL.bindVertices vb
            GL.bindBuffer GL.ElementArrayBuffer $= Just eb
    return $ \appInfo -> GLU.withVAO vao $
        do GL.currentProgram $= Just (GLU.program s)
           VGL.setUniforms s (rcast appInfo :: FieldRec CamInfo)
           GLU.drawIndexedTris 12
  where light :: SField '("lightDir", V3 GLfloat)
        light = SField

