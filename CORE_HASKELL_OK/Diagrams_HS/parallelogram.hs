{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import System.Random
import Diagrams.TwoD.Vector
import Linear.Affine
import Data.List ( sortBy )
import Data.Ord ( comparing )

u = r2 (1,2)
v = 2 *^ (unitY # rotateBy (1/12))
p = project u v

drawV v = fromOffsets [v]

example = mconcat 
	[ drawV p # lc green # lwG 0.03
	, drawV u # lc blue
	, drawV v # lc green
	, drawV (p ^-^ v) # translate v # dashingG [0.1,0.1] 0
	]

node    = circle 0.2 # fc green	
example2 = atPoints (trailVertices $ regPoly 6 1) (repeat node)

-- example3 = parallelogram v1 v2 v3 # fc black

--pts :: [P2 Double]
--pts = nonagon 1
--example3 = atPoints pts (repeat $ circle 0.1 # fc green)

example4 = flip atPoints (repeat (circle 0.2 # fc green)) $ map p2 $ [(1,1), (0,3), (-2,1), (-1,-4), (2,0)]

type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Polygon   = [Main.Point]

--let rn a b = randomRIO(a,b)
let q = randomRIO(0.1,2.0)
--let a = V2 q q
type Zeta = (Float, Float)
 
example5 = flip atPoints (repeat (circle 0.2 # fc green)) $ map p2 $ [(1,1), (0,3), (-2,1), (-1,-4), (2,0)]
take 10 $ (repeat) $ map p2 $ [(1,1), (0,3), (-2,1), (-1,-4), (2,0)]

main = mainWith (example4 :: Diagram B)
