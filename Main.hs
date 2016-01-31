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

import Geometry (buildBoard, buildNote)
import Window (initGL, InputState(..))

type Viewport = '("viewport", V2 GLsizei)

type RenderContext = FieldRec [ '("modelViewProj", M44 GLfloat), Viewport ]

data Renderables = Renderables {
        renderBoard :: RenderContext -> IO (),
        renderNote :: RenderContext -> IO ()
    }

main :: IO ()
main = do
    -- Create the window and store the window upate function
    updateWindow <- initGL "Programmer Hero" 1024 768

    -- Set up rendering settings
    GL.clearColor $= GL.Color4 0.1 0.1 0.6 1
    GL.depthFunc $= Just GL.Lequal
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

    -- Build scene and store entity render functions
    renderables <- Renderables <$> buildBoard <*> buildNote

    -- Kick off the main loop
    mainLoop camera updateWindow renderables
    where
        -- Render Function
        render :: Renderables -> RenderContext -> IO ()
        render renderables cxt = do
            renderBoard renderables cxt

        -- Main Loop
        mainLoop :: Camera.Camera GLfloat -> IO InputState -> Renderables -> IO ()
        mainLoop c updateWindow renderables = do
            windowState <- updateWindow

            -- Clear framebuffer
            GL.clear [GL.ColorBuffer, GL.DepthBuffer]

            -- Calculate view and projection matrices
            let V2 ww wh = fromIntegral <$> (windowSize windowState)
            let projMatrix = Camera.projectionMatrix (Camera.deg2rad 30) (ww / wh) 0.01 100
            let viewMatrix = Camera.camMatrix c

            -- Draw using render parameters
            render renderables (SField =: (projMatrix !*! viewMatrix) <+>
                                SField =: (fromIntegral <$> windowSize windowState))

            -- Quit if escaped has been pressed
            if S.member Key'Escape (keysPressed windowState)
            then return () -- terminate
            else mainLoop c updateWindow renderables
        camera = Camera.tilt (-20) $ Camera.dolly (V3 0 2 8) Camera.fpsCamera

