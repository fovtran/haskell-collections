{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

example = circle 1 # fc red # lw none ||| circle 1 # fc green # lw none


main = mainWith (example :: Diagram B)
