{-# LANGUAGE DataKinds, TypeOperators #-}
import Data.Vinyl
import Graphics.UI.GLFW (Key(Key'Escape))
import Linear (V2(..), V3(..), M44, (!*!))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=), GLfloat, GLint, GLsizei)
import qualified Graphics.GLUtil as GLU
import qualified Graphics.GLUtil.Camera3D as Camera
import qualified Graphics.VinylGL as VGL
import qualified Data.Set as S

import Geometry (buildCube, buildBoard)
import Window (initGL, InputState(..))

type Viewport = '("viewport", V2 GLsizei)

type AppInfo = FieldRec [ '("modelViewProj", M44 GLfloat),
                          Viewport ]

setup :: IO (AppInfo -> IO ())
setup = do
    -- Set up rendering settings
    GL.clearColor $= GL.Color4 0.1 0.1 0.6 1
    GL.depthFunc $= Just GL.Lequal
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

    -- Build scene
    renderAll <- (((.) sequence_) . sequence) <$> sequence [buildBoard]
    return renderAll

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
            render (SField =: (projMatrix !*! viewMatrix) <+>
                    SField =: (fromIntegral <$> windowSize windowState))

            -- Quit if escaped has been pressed
            if S.member Key'Escape (keysPressed windowState)
            then return () -- terminate
            else go c updateWindow render
        camera = Camera.tilt (-20) $ Camera.dolly (V3 0 2 8) Camera.fpsCamera

