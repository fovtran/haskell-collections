choose :: (Integral a) => a -> a -> a
choose n k = product [k+1..n] `div` product [1..n-k]
 
5 `choose` 3

-- Or, generate the binomial coefficients iteratively to avoid computing with big numbers:
choose :: (Integral a) => a -> a -> a
choose n k = foldl (\z i -> (z * (n-i+1)) `div` i) 1 [1..k]
 
-- Or using "caching":
coeffs = iterate next [1] 
  where
    next ns = zipWith (+) (0:ns) $ ns ++ [0]
 
main = print $ coeffs !! 5 !! 3