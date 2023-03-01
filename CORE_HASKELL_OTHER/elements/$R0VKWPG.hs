-- ghc -O2 --make A.hs
import Network
import Control.Concurrent
import System.IO
 
main :: IO()
main = do
  putStrLn "Please enter your name:"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ ", how are you?")