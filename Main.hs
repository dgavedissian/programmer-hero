{-# LANGUAGE DataKinds, TypeOperators #-}
import Data.Vinyl
import Graphics.UI.GLFW (Key(Key'Escape))
import Linear (V2(..), V3(..), M44, (!*!))
import System.FilePath ((</>))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=), GLfloat, GLint, GLsizei)
import qualified Graphics.GLUtil as GLU
import qualified Graphics.GLUtil.Camera3D as Camera
import qualified Graphics.VinylGL as VGL
import qualified Data.Set as S

import Geometry (buildCube)
import Window (initGL, InputState(..))

-- Note that the field name, "vertexCoord", matches the attribute name
-- in the vertex shader.
pos :: SField '("vertexCoord", v GLfloat)
pos = SField

tex :: SField '("tex", GLint)
tex = SField

logo :: IO ()
logo = do
    Right t <- GLU.readTexture ("textures"</>"logo.png")
    s <- GLU.simpleShaderProgram ("shaders"</>"logo.vert") ("shaders"</>"logo.frag")
    vb <- VGL.bufferVertices $ map (pos =:) [0, V2 0.25 0, 0.25, V2 0 0.25]
    vao <- GLU.makeVAO $ do
        GL.currentProgram $= Just (GLU.program s)
        VGL.enableVertices' s vb
        VGL.bindVertices vb
        GL.textureBinding GL.Texture2D $= Just t
        GL.textureFilter GL.Texture2D $= 
          ((GL.Nearest, Nothing), GL.Nearest)
        GLU.texture2DWrap $= (GL.Mirrored, GL.ClampToEdge)
        VGL.setUniforms s (tex =: 0)
    GLU.withVAO vao $ do
        GL.currentProgram $= Just (GLU.program s)
        GLU.withTextures2D [t] (GL.drawArrays GL.TriangleFan 0 4)

type Viewport = '("viewport", V2 GLsizei)

type AppInfo = FieldRec [ '("modelView", M44 GLfloat)
                        , '("proj", M44 GLfloat)
                        , Viewport ]

setup :: IO (AppInfo -> IO ())
setup = do
    -- Set up rendering settings
    GL.clearColor $= GL.Color4 0.1 0.1 0.6 1
    GL.depthFunc $= Just GL.Lequal
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

    -- Build scene
    renderAll <- (((.) sequence_) . sequence) <$> sequence [buildCube]

    -- Return a render function
    return $ \params -> do
        logo
        renderAll params

main :: IO ()
main = do
    -- Create the window and store the window upate function
    updateWindow <- initGL "Programmer Hero" 1024 768
    -- Set up the scene and store the render function
    render <- setup
    -- Kick off the main loop
    go camera updateWindow render
    where
        go :: Camera.Camera GLfloat -> IO InputState -> (AppInfo -> IO ()) -> IO ()
        go c updateWindow render = do
            windowState <- updateWindow

            -- Clear framebuffer
            GL.clear [GL.ColorBuffer, GL.DepthBuffer]

            -- Calculate view and projection matrices
            let V2 ww wh = fromIntegral <$> (windowSize windowState)
            let projMatrix = Camera.projectionMatrix (Camera.deg2rad 30) (ww / wh) 0.01 100
            let viewMatrix = Camera.camMatrix c

            -- Draw using render parameters
            render (SField =: viewMatrix <+> SField =: (projMatrix !*! viewMatrix) <+>
                    SField =: (fromIntegral <$> windowSize windowState))

            -- Quit if escaped has been pressed
            if S.member Key'Escape (keysPressed windowState)
            then return () -- terminate
            else go c updateWindow render
        camera = Camera.tilt (-20) $ Camera.dolly (V3 0 2 8) Camera.fpsCamera

