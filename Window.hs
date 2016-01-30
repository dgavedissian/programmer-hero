-- | Open a window and get an OpenGL context.
module Window (InputState(..), initGL, terminate) where
import Prelude hiding (init)
import Control.Monad (when)
import Data.IORef
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time.Clock
import Graphics.UI.GLFW
import Linear
import System.Directory (getCurrentDirectory, setCurrentDirectory)

data InputState = InputState {
        timeStep       :: Double,            -- Time in seconds since last frame
        keysPressed    :: Set Key,           -- All keys currently pressed
        buttonsPressed :: Set MouseButton,   -- All mouse buttons currently pressed
        mousePos       :: V2 Double,         -- Current mouse position
        windowSize     :: V2 Int             -- Current window size 
    }

-- Function called when a key is pressed or released
keyCallback :: IORef (Set Key) -> KeyCallback
keyCallback keys _w k _ KeyState'Pressed _mods = modifyIORef' keys (S.insert k)
keyCallback keys _w k _ KeyState'Released _mods = modifyIORef' keys (S.delete k)
keyCallback _ _ _ _ _ _ = return ()

-- Function called when a mouse button is pressed or released
mbCallback :: IORef (Set MouseButton) -> MouseButtonCallback
mbCallback mbs _w b MouseButtonState'Pressed _ = modifyIORef' mbs (S.insert b)
mbCallback mbs _w b MouseButtonState'Released _ = modifyIORef' mbs (S.delete b)

-- Function called when the mouse cursor is moved
mpCallback :: IORef (V2 Double) -> CursorPosCallback
mpCallback mp _w x y = writeIORef mp (V2 x y)

-- Function called when the window is resized
wsCallback :: IORef (V2 Int) -> WindowSizeCallback
wsCallback ws _w w h = writeIORef ws (V2 w h)

-- | @initGL windowTitle width height@ creates a window with the given
-- title and dimensions. The action returned presents a new frame (by
-- performing a buffer swap) and produces an updated snapshot of the
-- user interface.
initGL :: String -> Int -> Int -> IO (IO InputState)
initGL windowTitle width height = do
    setErrorCallback (Just simpleErrorCallback)
    r <- init
    when (not r) (error "Error initializing GLFW!")

    -- Pass in some hints to GLFW to create a core OpenGL profile
    windowHint $ WindowHint'ClientAPI ClientAPI'OpenGL
    windowHint $ WindowHint'OpenGLForwardCompat True
    windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
    windowHint $ WindowHint'ContextVersionMajor 3
    windowHint $ WindowHint'ContextVersionMinor 2

    -- Create a window, and store the context
    m@(~(Just w)) <- createWindow width height windowTitle Nothing Nothing
    when (isNothing m) (error "Couldn't create window!")
    makeContextCurrent m

    -- Set up kb/mouse state IORefs
    kbState <- newIORef S.empty
    mbState <- newIORef S.empty
    mpState <- getCursorPos w >>= newIORef . uncurry V2
    wsState <- getWindowSize w >>= newIORef . uncurry V2
    lastTick <- getCurrentTime >>= newIORef

    -- Set up event callbacks with the state refs
    setKeyCallback w (Just $ keyCallback kbState)
    setMouseButtonCallback w (Just $ mbCallback mbState)
    setCursorPosCallback w (Just $ mpCallback mpState)
    setWindowSizeCallback w (Just $ wsCallback wsState)

    -- Return a function which should be used when the window is updated
    return $ do
        -- GLFW event pump
        swapBuffers w
        pollEvents

        -- Get the current time
        t <- getCurrentTime

        -- Read the lastTick IORef, and convert this into an offset from
        -- the current time
        dt <- realToFrac . diffUTCTime t <$> readIORef lastTick
        writeIORef lastTick t

        -- as readIORef * returns an IO (Set _), the <$>/<*> trick is
        -- needed to chain them together and unwrap the Sets
        InputState dt <$> readIORef kbState
                      <*> readIORef mbState
                      <*> readIORef mpState
                      <*> readIORef wsState
  where simpleErrorCallback e s = putStrLn $ unwords [show e, show s]
