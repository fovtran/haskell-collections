module Bindings (display, reshape, keyboardMouse) where

import Graphics.UI.GLUT
-- import Display

reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)

keyboardMouse :: KeyboardMouseCallback
keyboardMouse _key _state _modifiers _position = return ()

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do 
  clear [ColorBuffer]
  renderPrimitive Points $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  flush
