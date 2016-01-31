{-# LANGUAGE DataKinds, TypeOperators, RecursiveDo, BangPatterns #-}
import           Control.Monad
import           Data.IORef
import           Graphics.UI.GLFW
import           Linear
import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=), GLfloat)
import qualified Graphics.GLUtil.Camera3D as Camera

import qualified Constants as C
import           Geometry
import           Window (initGL, InputState(..), KeyDownEvent)
import           Music

-- Imports for game logic
import Control.Concurrent
import Data.List
import Data.Time
import Data.Maybe
import Sound.MIDI.Message.Channel
import System.MIDI
import System.Random
import System.Environment
import qualified Data.EventList.Relative.TimeBody as EL
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.File.Event            as FE
import qualified Sound.MIDI.File.Load             as FL
import qualified Sound.MIDI.Message.Channel.Voice as MCV

type Music = [Note]

data Renderables = Renderables {
        renderBoard :: M44 GLfloat -> IO (),
        renderMarker :: M44 GLfloat -> M44 GLfloat -> V3 GLfloat -> IO (),
        renderNote :: M44 GLfloat -> M44 GLfloat -> V3 GLfloat -> IO ()
    }

data GameState = GameState {
        progress :: IORef Time,
        music :: IORef Music,
        finishingTime :: Time,
        renderables :: Renderables,
        projMatrix :: M44 GLfloat
    }

-- Turn from relative times to absolute times in the fst of the tuples
accumTimes :: Num t => [(t, a)] -> [(t, a)]
accumTimes xs
    = reverse . snd $ foldl' (\(!v, !acc) (!val, !a') ->
        (val + v, (val + v, a') : acc)) (0, []) xs

loadMusic :: FilePath -> IO Music
loadMusic file = do
    tracks <- F.getTracks <$> FL.fromFile file
    let events = sort $ concatMap (accumTimes . EL.toPairList) tracks
        filtered = fst $ foldl' (\(!acc, !lt) e@(!t, !ev)
                                    -> if lt < t
                                           then (e : acc, t)
                                           else (acc, lt)) ([], 0) events
    catMaybes <$!> mapM fromEvent filtered

fromEvent :: (FE.ElapsedTime, FE.T) -> IO (Maybe Note)
fromEvent (t, FE.MIDIEvent (Cons ch _)) = do
    r <- randomRIO (0, 3) :: IO Int
    let a = if r == 0 then 1 else 0
        ch' = (fromChannel ch + a) `mod` 4
    return $ Just $ Note (fromIntegral t / 1000, toEnum ch')
fromEvent _ = return Nothing

getLength :: Music -> Time
getLength notes = time
    where
        Note (time, _) = last notes

translateMatrix :: GLfloat -> GLfloat -> GLfloat -> M44 GLfloat
translateMatrix x y z = mkTransformationMat identity (V3 x y z)

scaleMatrix :: GLfloat -> GLfloat -> GLfloat -> M44 GLfloat
scaleMatrix x y z = V4 (V4 x 0 0 0) (V4 0 y 0 0) (V4 0 0 z 0) (V4 0 0 0 1)

-- Takes a list of notes which have not been missed already
checkHit :: Music -> Time -> Beat -> Bool
checkHit music elapsed beat = beat == b && (t - elapsed) < C.timeToCatch
    where
        Note (t, b) = head music

keyDownEvent :: GameState -> KeyDownEvent
keyDownEvent state key
    | key `elem` [Key'F1, Key'F2, Key'F3, Key'F4] = do
        musicState <- readIORef $ music state
        elapsed <- readIORef $ progress state
        when (checkHit musicState elapsed (mapKey key)) $
            modifyIORef' (music state) tail
    | otherwise = return ()
    where
        mapKey Key'F1 = F1
        mapKey Key'F2 = F2
        mapKey Key'F3 = F3
        mapKey Key'F4 = F4
        mapKey _ = error "This can never happen"

main :: IO ()
main = mdo
    -- Load the music
    [midiFile] <- getArgs
    (destination : _) <- enumerateDestinations
    conn <- openDestination destination
    start conn
    --_ <- playSingleThread song conn
    stop conn
    close conn
    music <- loadMusic midiFile

    -- Create the window and store the window upate function
    -- state doesn't actually exist at this point, but mdo saves us here so
    -- whatever
    updateWindow <- initGL "Programmer Hero" C.width C.height (keyDownEvent state)

    -- Set up rendering settings
    GL.clearColor $= C.backgroundColour
    GL.depthFunc $= Just GL.Lequal
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

    -- Build scene and store entity render functions
    renderables <- Renderables <$> buildBoard <*> buildMarker <*> buildNote

    -- Calculate the projection matrix
    let aspect = (fromIntegral C.width) / (fromIntegral C.height)
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

            let xoffset F1 = -1.5
                xoffset F2 = -0.5
                xoffset F3 = 0.5
                xoffset F4 = 1.5

            -- Render the markers for each colour
            forM_ [F1, F2, F3, F4] $ \note -> do
                let x = xoffset note * C.noteSpeed
                    modelMatrix = translateMatrix x 0.01 ((C.boardLength / 2) - (C.markerSize / 2))
                renderMarker (renderables state) viewProjMatrix modelMatrix (C.getBeatColours note)

            -- Drop notes which have already been played
            elapsed <- realToFrac <$> readIORef (progress state)
            modifyIORef' (music state) (filter (\(Note (t, _)) -> elapsed < t))

            -- Render a note for each note in the song
            currentMusic <- readIORef (music state)
            forM_ currentMusic $ \(Note (time, note)) -> do
                elapsed <- realToFrac <$> readIORef (progress state)
                let x = xoffset note * C.noteSpeed
                    distance = (elapsed - (realToFrac time)) * C.markerSize / C.timeToCatch
                    modelMatrix = (translateMatrix x 0.5 (distance + C.boardLength / 2))
                                   !*! (scaleMatrix 1 0.5 1)
                renderNote (renderables state) viewProjMatrix modelMatrix (C.getBeatColours note)

        -- Main Loop
        mainLoop :: Camera.Camera GLfloat -> IO InputState -> GameState -> IO ()
        mainLoop c updateWindow state = do
            windowState <- updateWindow

            -- UPDATE
            -- Increment the timer
            modifyIORef' (progress state) (+ timeStep windowState)

            -- RENDER
            -- Clear framebuffer
            GL.clear [GL.ColorBuffer, GL.DepthBuffer]

            -- Calculate view and projection matrices
            let viewMatrix = Camera.camMatrix c

            -- Draw using render parameters
            render state (projMatrix state !*! viewMatrix)

            -- Check win condition
            remMusic <- readIORef . music $ state
            when (null remMusic) $
              putStrLn "You won"

            -- Quit if escaped has been pressed
            unless (null remMusic || shouldQuit windowState) $
              mainLoop c updateWindow state
        camera = Camera.tilt (-20) $ Camera.dolly (V3 0 16 64) Camera.fpsCamera

