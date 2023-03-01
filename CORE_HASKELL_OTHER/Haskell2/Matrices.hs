# in Matrix.hs +import Prelude hiding (any, all, read, map, foldMap)
import Numeric.Matrix as M
import Data.Maybe 

create :: [[Float]] -> Matrix Float 

create m = M.fromList m

mahalanobisDistance mu x = (transpose (minus x mu)) 
`times` (fromJust (M.inv coVarMatrix)) 
`times` (minus x mu) 
where 
    coVarMatrix = create [[1.1, 0.3], [0.3, 1.9]]

distanceW1 = mahalanobisDistance (create [[1.0], [1.0]])

distanceW2 = mahalanobisDistance (create [[1.5], [1.5]])

main = do 
    let x = create [[1.0], [2.2]]
    print $ distanceW1 x