{-# LANGUAGE NoMonomorphismRestriction #-}
module Constants where

import Linear (V3(..))
import Graphics.Rendering.OpenGL (GLfloat, Color4(..))

import Music

-- Window Dimensions
width = 1024

height = 768

-- Background Colour
backgroundColour :: Color4 GLfloat
backgroundColour = Color4 0 0 0 1

-- Beat colours
getBeatColours :: Beat -> V3 GLfloat
getBeatColours F1 = V3 1 0 0
getBeatColours F2 = V3 0 1 0
getBeatColours F3 = V3 0 0 1
getBeatColours F4 = V3 1 1 0

-- Board Size
boardWidth = 16
boardLength = 80
markerSize = boardWidth / 4

-- Units per second
noteSpeed = 4
timeToCatch = 0.25
