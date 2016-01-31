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
    let width = 1024
        height = 768

    -- Create the window and store the window upate function
    updateWindow <- initGL "Programmer Hero" width height

    -- Set up rendering settings
    GL.clearColor $= GL.Color4 0.1 0.1 0.6 1
    GL.depthFunc $= Just GL.Lequal
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

    -- Build scene and store entity render functions
    renderables <- Renderables <$> buildBoard <*> buildNote

    -- Calculate the projection matrix
    let aspect = (fromIntegral width) / (fromIntegral height)
    let projMatrix = Camera.projectionMatrix (Camera.deg2rad 30) aspect 0.1 100

    -- Kick off the main loop
    mainLoop camera updateWindow renderables projMatrix
    where
        -- Render Function
        render :: Renderables -> RenderContext -> IO ()
        render renderables cxt = do
            renderBoard renderables cxt

        -- Main Loop
        mainLoop :: Camera.Camera GLfloat -> IO InputState -> Renderables -> M44 GLfloat -> IO ()
        mainLoop c updateWindow renderables projMatrix = do
            windowState <- updateWindow

            -- Clear framebuffer
            GL.clear [GL.ColorBuffer, GL.DepthBuffer]

            -- Calculate view and projection matrices
            let viewMatrix = Camera.camMatrix c

            -- Draw using render parameters
            render renderables (SField =: (projMatrix !*! viewMatrix) <+>
                                SField =: (fromIntegral <$> windowSize windowState))

            -- Quit if escaped has been pressed
            if S.member Key'Escape (keysPressed windowState)
            then return () -- terminate
            else mainLoop c updateWindow renderables projMatrix
        camera = Camera.tilt (-20) $ Camera.dolly (V3 0 2 8) Camera.fpsCamera

