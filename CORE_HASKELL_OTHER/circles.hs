import Graphics.UI.GLUT

type Point = (Float, Float)
type Polygon = [Point]

-- circle2 (x, y) radius divs = map toPoint angles where 
--    arc       = 2.0 * pi / fromIntegral divs
--    toPoint a = (x + cos a * radius, y + sin a * radius)
--    angles    = map ((*arc) . fromIntegral) [0..divs]

-- renderFan points = do
--    renderPrimitive TriangleFan $ mapM_ (\(x, y) -> vertex (Vertex2 x y)) points
renderCircle centre radius divs = renderFan (centre : circle centre radius divs)

circle :: Point -> Float -> Polygon
circle (x,y) r = map (\t -> (x+r*cos (t), y+r*sin (t))) [0,0.2..(2*pi)]

--            center  radius   
ball = circle (0,0.2) 0.2


points2GL :: [Point] -> [(GLfloat,GLfloat)]
points2GL l = [ (realToFrac x, realToFrac y) | (x,y) <- l ]

glPoints2Vertexes pts = mapM_ (\(x, y) -> vertex $ Vertex2 x y) pts

points2Vertexes pts = glPoints2Vertexes (points2GL pts)

renderPrimitive = Polygon (points2Vertexes ball)