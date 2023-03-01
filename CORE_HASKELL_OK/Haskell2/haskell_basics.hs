-- haskell type assignments

-- Number
x :: Integer
y :: Integer
z :: Integer

let r = 5.0
-- r :: Fractional r => r
let area = pi * r ^ 2


-- Functions

let double x    = 2 * x
let quadruple x = double (double x)
let square x    = x * x
let half   x    = x / 2

let areaRect l w = l * w
let areaSquare s = areaRect s s
let areaTriangle b h = (b * h) / 2

let tuple = (2, "Hi")

-- factorial function
let fac n = if n == 0 then 1 else n * fac (n-1)

roots a b c =
    let sdisc = sqrt (b * b - 4 * a * c)
        twice_a = 2 * a
    in  ((-b + sdisc) / twice_a,
         (-b - sdisc) / twice_a)
         
let max a b = if a > b then a else b

# functions
map (\x -> x ^ 2) [1..10]
-- currying
let comb n m = if m == 0 || m == n then 1 else comb (n-1) m + comb (n-1) (m-1)

let mycmd = getLine >>= \s -> return ("You entered: " ++ s)
mycmd >>= putStrLn

readFile "file.txt" >>= \s -> putStrLn (take 10 s)

B.hs:
import M1
main = putStrLn s

M1.hs:
module M1 where
s = "Hi, Everyone!"

let heron a b c = sqrt (s * (s - a) * (s - b) * (s - c))
    where
    s = (a + b + c) / 2