import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Renderable
import Graphics.UI.Gtk.Misc.DrawingArea
import qualified Graphics.UI.Gtk as G

main = do
        win  <- createRenderableWindow emptyRenderable 400 400
        let draw = castToDrawingArea win
        G.widgetShowAll win
        updateCanvas emptyRenderable draw
