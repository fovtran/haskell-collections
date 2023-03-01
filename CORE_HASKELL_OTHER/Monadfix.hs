module Main where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)

main :: IO ()
main = do