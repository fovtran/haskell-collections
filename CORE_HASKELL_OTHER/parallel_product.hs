{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

{-# OPTIONS_GHC -O2 -Wall -threaded -fforce-recomp #-}

import Criterion.Main
import Control.Monad (when)
import Control.Parallel.Strategies (runEval,rpar,rseq)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar,takeMVar,putMVar)
import System.IO.Unsafe (unsafePerformIO)
import GHC.IO (evaluate)
import qualified Data.Vector.Primitive as PV

main :: IO ()
main = do
  let expected = PV.product numbers
  when (not (serialProduct numbers == expected)) $ do
    fail "serialProduct implementation incorrect"
  defaultMain
    [ bgroup "product"
      [ bench "serial" $ whnf serialProduct numbers
      , bench "parallel" $ whnf parallelProduct numbers
      , bench "parallel mvar" $ whnf parallelProductFork numbers
      ]
    ]

numbers :: PV.Vector Double
numbers = PV.replicate 10000000 1.00000001
{-# NOINLINE numbers #-}

serialProduct :: PV.Vector Double -> Double
serialProduct v =
  let !len = PV.length v
      go :: Double -> Int -> Double
      go !d !ix = if ix < len then go (d * PV.unsafeIndex v ix) (ix + 1) else d
   in go 1.0 0

-- | This only works when the vector length is a multiple of 8.
parallelProduct :: PV.Vector Double -> Double
parallelProduct v = runEval $ do
  let chunk = div (PV.length v) 8
  p2 <- rpar (serialProduct (PV.slice (chunk * 6) chunk v))
  p3 <- rpar (serialProduct (PV.slice (chunk * 7) chunk v))
  p1 <- rseq (serialProduct (PV.slice (chunk * 0) (chunk * 6) v))
  return (p1 * p2 * p3)

-- | This only works when the vector length is a multiple of 4.
parallelProductFork :: PV.Vector Double -> Double
parallelProductFork v = unsafePerformIO $ do
  let chunk = div (PV.length v) 4
  var <- newEmptyMVar 
  _ <- forkIO $ evaluate (serialProduct (PV.slice (chunk * 0) chunk v)) >>= putMVar var
  _ <- forkIO $ evaluate (serialProduct (PV.slice (chunk * 1) chunk v)) >>= putMVar var
  _ <- forkIO $ evaluate (serialProduct (PV.slice (chunk * 2) chunk v)) >>= putMVar var
  _ <- forkIO $ evaluate (serialProduct (PV.slice (chunk * 3) chunk v)) >>= putMVar var
  a <- takeMVar var
  b <- takeMVar var
  c <- takeMVar var
  d <- takeMVar var
  return (a * b * c * d)