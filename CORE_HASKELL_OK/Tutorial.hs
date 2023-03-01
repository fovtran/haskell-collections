>>> :t 'a'
'a' :: Char
>>> :t True
True :: Bool
>>> :t "HELLO!"  
"HELLO!" :: [Char]
>>> :t (True, 'a') 
(True, 'a') :: (Bool, Char)
>>> :t 4 == 5  
4 == 5 :: Bool

Prelude> a = [1..9]
Prelude> a
[1,2,3,4,5,6,7,8,9]
Prelude> [x ^2 | x <- [1..10]]
[1,4,9,16,25,36,49,64,81,100]
Prelude> (head a,tail a,take 2 a, a !! 2)
(1,[2,3,4,5,6,7,8,9],[1,2],3)

Prelude> class (Real a, Enum a) => Integral a
Prelude> factorial :: (Integral a) => a -> a
factorial n = product [1..n]

circumference r = 2 * pi * r  
circumference :: Floating a => a -> a

circumference 4.0

:set +s
:r
:show bindings 

import Data.List
