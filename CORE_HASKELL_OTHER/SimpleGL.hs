import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

main = do
	(progName,_) <-getArgsAndInitialize
	createAWindow progName
	mainLoop

createAWindow windowName = do
	createWindow windowName
	displayCallback $= displayPoints

displayPoints = do
	clear [ColorBuffer]
	renderPrimitive Polygon
	$mapM_ (\(x, y, z)->vertex$Vertex3 x y z) myPoints

myPoints :: [(GLfloat,GLfloat,GLfloat)]

myPoints =
	[(-0.25, 0.25, 0.0)
	,(0.75, 0.35, 0.0)
	,(0.75, -0.15, 0.0)
	,((-0.75), -0.25, 0.0)]
