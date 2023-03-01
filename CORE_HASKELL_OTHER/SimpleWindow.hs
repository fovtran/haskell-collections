import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

main = do
	getArgsAndInitialize
	myWindow "Hello Window"
	mainLoop

myWindow name = do
	createWindow name
	windowSize $= Size 800 500
	displayCallback $= clear [ColorBuffer]
