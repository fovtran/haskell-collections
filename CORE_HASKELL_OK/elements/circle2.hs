{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
-- or:
-- import Diagrams.Backend.xxx.CmdLine
-- where xxx is the backend you would like to use.

myCircle :: Diagram B
myCircle = circle 1

main = mainWith myCircle
