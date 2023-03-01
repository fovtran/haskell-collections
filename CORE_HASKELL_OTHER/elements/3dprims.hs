-- image at https://s3.amazonaws.com/bergey/diagrams/3dprims.png

import Diagrams.ThreeD
import Diagrams.Prelude hiding (unitX, unitY, unit_X, unit_Y, direction)
import Diagrams.Backend.POVRay

cam = mm50Camera # translate (r3 (0,0,5))

_xy :: Spherical Rad
_xy = direction . r3 $ (-1, -1, -0.5)

light = parallelLight _xy white

example :: Diagram POVRay R3
example = mconcat [cam, cube # fc red # translate unit_X # transform (aboutY (0.1 :: Turn))
                  , cone # fc blue # translate unitX, light]

main :: IO ()
main = putStrLn $ renderDia POVRay POVRayOptions example
