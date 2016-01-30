module Display (display) where
 
import Graphics.UI.GLUT
import Cube
 
display :: DisplayCallback
display = do 
  clear [ColorBuffer]
  cube 0.2
  flush
