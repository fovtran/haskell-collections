import System.TimeIt

import Data.Vector as V

vector :: IO (Vector Int)
vector = do
  let vec = V.replicate 3000000 10
  print $ V.length vec
  return vec

sumit :: IO ()
sumit = do
  vec <- vector
  print $ V.sum vec

time = timeIt sumit