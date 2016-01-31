{-# LANGUAGE DataKinds, TypeOperators, RecursiveDo #-}
import           Control.Monad
import           Data.Vinyl
import           Data.IORef
import           Graphics.UI.GLFW
import           Linear
import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=), GLfloat, GLint, GLsizei)
import qualified Graphics.GLUtil as GLU
import qualified Graphics.GLUtil.Camera3D as Camera
import qualified Graphics.VinylGL as VGL
import qualified Data.Set as S

import Geometry (buildBoard, buildNote)
import Window (initGL, InputState(..), KeyDownEvent)
import Music

type Viewport = '("viewport", V2 GLsizei)

type Music = [Note]

data Renderables = Renderables {
        renderBoard :: M44 GLfloat -> IO (),
        renderNote :: M44 GLfloat -> M44 GLfloat -> IO ()
    }

data GameState = GameState {
        progress :: IORef Time,
        music :: IORef Music,
        finishingTime :: Time,
        renderables :: Renderables,
        projMatrix :: M44 GLfloat
    }

-- Stub method
loadMusic :: Music
loadMusic = [
        Note (2, F1),
        Note (2.5, F2),
        Note (3, F3),
        Note (3.5, F4),
        Note (4, F3),
        Note (5.5, F2),
        Note (6, F1),

        Note (10, F1),
        Note (11, F2),
        Note (12, F3),
        Note (13, F4),
        Note (14, F3),
        Note (15, F2),
        Note (16, F1)
    ]

getLength :: Music -> Time
getLength notes = time
    where
        Note (time, _) = last notes

translate :: GLfloat -> GLfloat -> GLfloat -> M44 GLfloat
translate x y z = mkTransformationMat identity (V3 x y z)

-- Takes a list of notes which have not been missed already
checkHit :: Music -> Time -> Beat -> Bool
checkHit music elapsed beat = beat == b && (t - elapsed) < 1
    where
        Note (t, b) = head music

keyDownEvent :: GameState -> KeyDownEvent
keyDownEvent state key
    | key `elem` [Key'F1, Key'F2, Key'F3, Key'F4] = do
        musicState <- readIORef $ music state
        elapsed <- readIORef $ progress state
        if checkHit musicState elapsed (mapKey key) then
            modifyIORef' (music state) (drop 1)
        else
            return () -- miss
    | otherwise = return ()
    where
        mapKey Key'F1 = F1
        mapKey Key'F2 = F2
        mapKey Key'F3 = F3
        mapKey Key'F4 = F4
        mapKey _ = error "This can never happen"

main :: IO ()
main = mdo
    let width = 1024
        height = 768

    -- Load the music
    let music = loadMusic

    -- Create the window and store the window upate function
    -- state doesn't actually exist at this point, but mdo saves us here so
    -- whatever
    updateWindow <- initGL "Programmer Hero" width height (keyDownEvent state)

    -- Set up rendering settings
    GL.clearColor $= GL.Color4 0.1 0.1 0.6 1
    GL.depthFunc $= Just GL.Lequal
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

    -- Build scene and store entity render functions
    renderables <- Renderables <$> buildBoard <*> buildNote
    
    -- Calculate the projection matrix
    let aspect = (fromIntegral width) / (fromIntegral height)
    let projMatrix = Camera.projectionMatrix (Camera.deg2rad 30) aspect 0.1 1000
    
    -- Set up the game state
    musicRef <- newIORef music
    progressRef <- newIORef 0.0
    state <- return $ GameState progressRef musicRef (getLength music) renderables projMatrix

    -- Kick off the main loop
    mainLoop camera updateWindow state
    where
        -- Render Function
        render :: GameState -> M44 GLfloat -> IO ()
        render state viewProjMatrix = do
            renderBoard (renderables state) viewProjMatrix

            -- Drop notes which have already been played
            elapsed <- realToFrac <$> readIORef (progress state)
            modifyIORef' (music state) (filter (\(Note (t, _)) -> elapsed < t))

            -- Render a note for each note in the song
            currentMusic <- readIORef (music state)
            forM_ currentMusic $ \(Note (time, note)) -> do
                elapsed <- realToFrac <$> readIORef (progress state)
                let xoffset F1 = -1.5
                    xoffset F2 = -0.5
                    xoffset F3 = 0.5
                    xoffset F4 = 1.5
                    x = (xoffset note) * 4
                    modelMatrix = translate x 0 ((elapsed - (realToFrac time)) * 16 + 40)
                renderNote (renderables state) viewProjMatrix modelMatrix

        -- Main Loop
        mainLoop :: Camera.Camera GLfloat -> IO InputState -> GameState -> IO ()
        mainLoop c updateWindow state = do
            windowState <- updateWindow

            -- UPDATE
            -- Increment the timer
            modifyIORef' (progress state) (+(timeStep windowState))

            -- RENDER
            -- Clear framebuffer
            GL.clear [GL.ColorBuffer, GL.DepthBuffer]

            -- Calculate view and projection matrices
            let viewMatrix = Camera.camMatrix c

            -- Draw using render parameters
            render state ((projMatrix state) !*! viewMatrix)

            -- Quit if escaped has been pressed
            if shouldQuit windowState
            then return () -- terminate
            else mainLoop c updateWindow state
        camera = Camera.tilt (-20) $ Camera.dolly (V3 0 16 64) Camera.fpsCamera

