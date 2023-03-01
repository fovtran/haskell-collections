{-# LANGUAGE DeriveDataTypeable #-}
module Lib6 where

import Data.Data
import Data.Generics.Uniplate.Data
import Text.Show.Pretty (ppShow)

data LogicExp  a = P a                              |
                     True'                      |
                     False'                                 |
                     Not' (LogicExp a)                  |
                     (LogicExp a) :&  (LogicExp a)  |
                     (LogicExp a) :|  (LogicExp a)  |
                     (LogicExp a) :=> (LogicExp a)    |
                     (LogicExp a) :=  (LogicExp a)
    deriving (Show, Data, Typeable)

type LExp = LogicExp String

data Position = L | R

deMorgan :: LExp -> LExp
deMorgan (e1 :& e2) = Not' ((Not' e1) :| (Not' e2))
deMorgan (e1 :| e2) = Not' ((Not' e1) :& (Not' e2))
deMorgan x = x

doit :: LExp -> LExp
doit = transform deMorgan

example = (P "a" :& P "b") :| (P "c")

test = putStrLn $ ppShow (doit example)
Running test produces:

Not' (Not' (Not' (Not' (P "a") :| Not' (P "b"))) :& Not' (P "c"))