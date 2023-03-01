import Diagrams.Backend.SVG (renderSVG)

outputFile :: FilePath
outputFile = "strukturine.svg"

dimensions :: SizeSpec2D
dimensions = mkSizeSpec (Just 400) (Just 400)

-- renderSVG :: FilePath -> SizeSpec2D -> Diagram SVG R2 -> IO ()
strukturineDiagram :: Diagram SVG R2

strukturine = do renderSVG outputFile dimensions strukturineDiagram	
