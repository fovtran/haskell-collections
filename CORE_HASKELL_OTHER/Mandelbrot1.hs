import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

complex :: Float -> Float -> Complex
complex x y = C x y

real :: Complex -> Float
real (C x y)    = x

im :: Complex -> Float
im   (C x y)    = y

magnitude :: Complex -> Float
magnitude = real.abs

main :: IO ()
main = do
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "Mandelbrot Set with Haskell and OpenGL"
  -- Each time we will need to update the display we will call the function 'display'
  displayCallback $= display
  mainLoop

display = do
  clear [ColorBuffer] -- make the window black
  loadIdentity -- reset any transformation
  preservingMatrix drawMandelbrot
  swapBuffers -- refresh screen

drawMandelbrot =
  -- We will print Points (not triangles for example)
  renderPrimitive Points $ do
    mapM_ drawColoredPoint allPoints
  where
      drawColoredPoint (x,y,c) = do
          color c -- set the current color to c
          -- then draw the point at position (x,y,0)
          -- remember we're in 3D
          vertex $ Vertex3 x y 0

drawMandelbrot =
  renderPrimitive Points $ do
    color color1
    vertex $ Vertex3 x1 y1 0
    color colorN
    vertex $ Vertex3 xN yN 0


width = 320 :: GLfloat
height = 320 :: GLfloat

allPoints :: [(GLfloat,GLfloat,Color3 GLfloat)]
allPoints = [ (x/width,y/height,colorFromValue $ mandel x y) |
                  x <- [-width..width],
                  y <- [-height..height]]

colorFromValue n =
  let
      t :: Int -> GLfloat
      t i = 0.5 + 0.5*cos( fromIntegral i / 10 )
  in
    Color3 (t n) (t (n+5)) (t (n+10))

mandel x y =
  let r = 2.0 * x / width
      i = 2.0 * y / height
  in
      f (complex r i) 0 64

f :: Complex -> Complex -> Int -> Int
f c z 0 = 0
f c z n = if (magnitude z > 2 )
          then n
else f c ((z*z)+c) (n-1)

