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
type PlaybackData = [(Double, FE.T)]

data Renderables = Renderables {
        renderBoard :: M44 GLfloat -> IO (),
        renderMarker :: M44 GLfloat -> M44 GLfloat -> V3 GLfloat -> IO (),
        renderNote :: M44 GLfloat -> M44 GLfloat -> V3 GLfloat -> IO ()
    }

data GameState = GameState {
        progress :: IORef Time,
        music :: IORef Music,
        playback :: IORef PlaybackData,
        connection :: Maybe Connection,
        finishingTime :: Time,
        f1Size :: IORef GLfloat,
        f2Size :: IORef GLfloat,
        f3Size :: IORef GLfloat,
        f4Size :: IORef GLfloat,
        renderables :: Renderables,
        projMatrix :: M44 GLfloat
    }

-- Turn from relative times to absolute times in the fst of the tuples
accumTimes :: Num t => [(t, a)] -> [(t, a)]
accumTimes xs
    = reverse . snd $ foldl' (\(!v, !acc) (!val, !a') ->
        (val + v, (val + v, a') : acc)) (0, []) xs

loadMusic :: FilePath -> IO (Music, PlaybackData)
loadMusic file = do
    tracks <- F.getTracks <$> FL.fromFile file
    let events = sort $ concatMap (accumTimes . EL.toPairList) tracks
        filtered = fst $ foldl' (\(!acc, !lt) e@(!t, !ev)
                                    -> if lt + 100 < t
                                           then (e : acc, t)
                                           else (acc, lt)) ([], 0) events
    fs <- catMaybes <$!> mapM fromEvent filtered
    return (fs, map (\(f, s) -> (fromIntegral f / 1000, s)) events)

fromEvent :: (FE.ElapsedTime, FE.T) -> IO (Maybe Note)
fromEvent (t, FE.MIDIEvent (Cons ch _)) = do
    r <- randomRIO (0, 3) :: IO Int
    let a = if r == 0 then 1 else 0
        ch' = (fromChannel ch + a) `mod` 4
    return $ Just $ Note (fromIntegral t / 1000, toEnum ch')
fromEvent _ = return Nothing

playNote :: Maybe Connection -> FE.T -> IO ()
playNote (Just connection) (FE.MIDIEvent (Cons ch body))
    = play ch body connection
playNote _ _ = return ()

play :: Channel -> Body -> Connection -> IO ()
play ch (Voice (MCV.NoteOn p v)) conn
    = send conn (MidiMessage (fromChannel ch) (NoteOn (fromPitch p) (fromVelocity v)))
play ch (Voice (MCV.NoteOff p v)) conn
    = send conn (MidiMessage (fromChannel ch) (NoteOff (fromPitch p) (fromVelocity v)))
play _ _ _ = return ()


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
        enlargeMarker key
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
        mapKey _ = undefined
        enlargeMarker Key'F1 = writeIORef (f1Size state) C.markerScale
        enlargeMarker Key'F2 = writeIORef (f2Size state) C.markerScale
        enlargeMarker Key'F3 = writeIORef (f3Size state) C.markerScale
        enlargeMarker Key'F4 = writeIORef (f4Size state) C.markerScale
        enlargeMarker _ = undefined

main :: IO ()
main = mdo
    -- Load the music
    [midiFile] <- getArgs
    ds <- enumerateDestinations
    conn <- case ds of
        (destination : _) -> do
            c <- openDestination destination
            start c
            close c
            return $ Just c
        _ -> return Nothing
    (music, playback) <- loadMusic midiFile

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
    playbackRef <- newIORef playback
    f1Ref <- newIORef 1.0
    f2Ref <- newIORef 1.0
    f3Ref <- newIORef 1.0
    f4Ref <- newIORef 1.0
    let state = GameState progressRef
                          musicRef
                          playbackRef
                          conn
                          (getLength music)
                          f1Ref
                          f2Ref
                          f3Ref
                          f4Ref
                          renderables
                          projMatrix

    -- Kick off the main loop
    mainLoop camera updateWindow state
    case conn of
        Just c -> do
            stop c
            close c
        _ -> return ()
    where
        -- Render Function
        render :: GameState -> M44 GLfloat -> IO ()
        render state viewProjMatrix = do
            renderBoard (renderables state) viewProjMatrix

            let xoffset F1 = -1.5
                xoffset F2 = -0.5
                xoffset F3 = 0.5
                xoffset F4 = 1.5
                scaleDown x = (x - 1.0) * 0.9 + 1.0

            -- Render the markers for each colour
            modifyIORef' (f1Size state) scaleDown
            modifyIORef' (f2Size state) scaleDown
            modifyIORef' (f3Size state) scaleDown
            modifyIORef' (f4Size state) scaleDown
            f1CurSize <- readIORef (f1Size state)
            f2CurSize <- readIORef (f2Size state)
            f3CurSize <- readIORef (f3Size state)
            f4CurSize <- readIORef (f4Size state)
            forM_ [F1, F2, F3, F4] $ \note -> do
                let x = xoffset note * C.markerRegion
                    scale F1 = f1CurSize
                    scale F2 = f2CurSize
                    scale F3 = f3CurSize
                    scale F4 = f4CurSize
                    scaleSize = scale note
                    modelMatrix = (translateMatrix x 0.01 ((C.boardLength / 2) - (C.markerSize / 2))) !*! (scaleMatrix scaleSize scaleSize scaleSize)
                renderMarker (renderables state) viewProjMatrix modelMatrix (C.getBeatColours note)

            -- Drop notes which have already been played
            elapsed <- realToFrac <$> readIORef (progress state)
            modifyIORef' (music state) (dropWhile (\(Note (t, _)) -> elapsed >= t))

            -- Render a note for each note in the song
            currentMusic <- readIORef (music state)
            forM_ currentMusic $ \(Note (time, note)) -> do
                elapsed <- realToFrac <$> readIORef (progress state)
                let x = xoffset note * C.noteSpeed
                    distance = (elapsed - (realToFrac time)) * C.markerSize / C.timeToCatch
                    modelMatrix = translateMatrix x 0 (distance + C.boardLength / 2 - C.markerRegion / 2)
                renderNote (renderables state) viewProjMatrix modelMatrix (C.getBeatColours note)

        -- Main Loop
        mainLoop :: Camera.Camera GLfloat -> IO InputState -> GameState -> IO ()
        mainLoop c updateWindow state = do
            windowState <- updateWindow

            -- UPDATE
            -- Increment the timer
            modifyIORef' (progress state) (+ timeStep windowState)

            progress' <- readIORef (progress state)
            playAllDue progress' state

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
        playAllDue :: Time -> GameState -> IO ()
        playAllDue progress state = do
            es <- readIORef (playback state)
            let (abstime, event) = head es
            when (abstime <= progress) $ do
                playNote (connection state) event
                modifyIORef' (playback state) tail
                playAllDue progress state

