{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (Int, foldl, fromInteger, (+), putStrLn, (.), ($), IO, show)
import Data.Matrix
import Data.Vector
import Data.Char

-- :m + Data.Vector
let a = fromList [10, 20, 30, 40]
-- let x = generate 10 (\n -> Data.Vector.replicate 10 n)
-- let y = generate 25 (\n -> n)

type Matrix= [[Int]]
type Pos = (Int,Int)
type Algebra f a = f a -> a

main = do 
	show "1"
