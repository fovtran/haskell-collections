module Main (main
            , computeArray) where

import Control.Monad
import Control.Monad.State (State, evalState, get, put)
import System.Random (StdGen, mkStdGen, random)
import Control.Applicative ((<$>))

type Rnd a = State StdGen a

runRandom :: Rnd a -> Int -> a
runRandom action seed = evalState action $ mkStdGen seed

rand :: Rnd Double
rand = do
  gen <- get
  let (r, gen') = random gen
  put gen'
  return r

{- Uniform distributions -}
uniform01 :: Rnd [Double]
uniform01 = mapM (\_ -> rand) $ repeat ()

{- Get n samples uniformly distributed between 0 and 1 -}
sampleUniform :: Int -> Rnd [Double]
sampleUniform n = liftM (take n) uniform01

computeArray :: Rnd [Bool]
computeArray = do
  samples1 <- sampleUniform 10
  samples2 <- sampleUniform 10
  let dat = zip samples1 samples2
  return $ uncurry (<) <$> dat

main :: IO ()
main = do
  let seed = 48
  let res = runRandom computeArray seed
  putStrLn $ show res