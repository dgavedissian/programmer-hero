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
        shouldQuit     :: Bool
    }

-- Function called when a key is pressed or released
keyCallback :: IORef Bool -> KeyCallback
keyCallback shouldQuit _ Key'Escape _ KeyState'Pressed _ = modifyIORef' shouldQuit (\_ -> True)
keyCallback _ _ k _ KeyState'Pressed _ = print k
keyCallback _ _ _ _ _ _ = return ()

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

    -- Set up state IORefs
    lastTick <- getCurrentTime >>= newIORef
    shouldQuit <- newIORef False

    -- Set up event callbacks
    setKeyCallback w (Just $ keyCallback shouldQuit)

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

        InputState dt <$> readIORef shouldQuit

    where
        simpleErrorCallback e s = putStrLn $ unwords [show e, show s]
