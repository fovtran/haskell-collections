data Complex = C Float Float deriving (Show,Eq)
instance Num Complex where
    fromInteger n = C (fromIntegral n,0.0)
    (C x y) * (C z t) = C (z*x - y*t) (y*z + x*t)
    (C x y) + (C z t) = C (x+z) (y+t)
    abs (C x y)     = C (sqrt (x*x + y*y)) 0.0
    signum (C x y)  = C (signum x) (0.0)