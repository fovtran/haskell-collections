{-# LANGUAGE NoImplicitPrelude #-}

import Data.Char
import Data.String
import Data.Matrix
import Data.Kinds
import Data.Vector

-- let a = fromList [10, 20, 30, 40]
-- let x = do generate 10 where (\n -> Data.Vector.replicate 10 n)
let y = generate 25 n <- n

type Matrix= do [[Int]]
type Pos = (Int,Int)
type Algebra f a = f a -> a

