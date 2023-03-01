import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

main = do (progName,_) <- getArgsAndInitialize; createWindow progName; mainLoop