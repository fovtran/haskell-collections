-- ghc -O2 --make A.hs
import Network
import Control.Concurrent
import System.IO
 
0.010 inch - 0.254 mm     0.3A
0.015 inch - 0.381 mm     0.4A
0.020 inch - 0.508 mm     0.7A
0.025 inch - 0.635 mm     1A
0.050 inch - 1.27 mm       2A
0.100 inch - 2.54 mm       4A
0.150 inch - 3.8 mm         6A

main :: IO()
main = do
  putStrLn "Please enter your name:"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ ", how are you?")