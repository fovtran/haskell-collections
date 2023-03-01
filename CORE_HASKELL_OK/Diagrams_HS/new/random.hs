g <- newStdGen
randomR (1, 10) g
randomRIO (1, 10)

import System.IO.Unsafe  -- be careful!                                         
import System.Random

-- a randomly chosen, program-scoped constant from the range [0 .. 9]            
c :: Int
c = unsafePerformIO (getStdRandom (randomR (0, 9)))

fmap yourFunctionX $ randomRIO (a, b)
||
fmap (\x -> yourFunctionX aParam x anotherParam) $ randomRIO (a, b)

import Control.Applicative


yourFunctionX <$> randomRIO (a, b)

(\x -> yourFunctionX aParam x anotherParam) <$> randomRIO (a, b)