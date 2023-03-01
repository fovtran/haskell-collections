{-# LANGUAGE NoMonomorphismRestriction #-}

module Strukturine where
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Maybe (fromMaybe)
import Data.Char
import Input
import qualified Input(getNumber) --other module

main = mainWith(strukturine :: Diagram B R2)