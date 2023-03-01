module Treeish where

-- This example requires the containers,
-- diagrams-core, diagrams-lib, diagrams-contrib and diagrams-svg packages
import Data.Tree
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree (renderTree,symmLayout',_slHSep,_slVSep)
import Diagrams.Backend.SVG (SVG)
import Diagrams.Backend.SVG.CmdLine (defaultMain)

exampleTree :: Tree String
exampleTree = Node "A" [Node "B" [], Node "C" []]

renderNodeTree :: Tree String -> QDiagram SVG V2 Double Any
renderNodeTree nodeTree = renderTree
    (\a -> letter a `atop` square 1.03 # fc white)
    (~~)
    (symmLayout' (with{ _slHSep = 3,  _slVSep = 2}) nodeTree)
  where
     letter a = text a # font "monospace" # fontSize (local 0.47)

main :: IO ()
main = defaultMain (renderNodeTree exampleTree)
