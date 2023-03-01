#!/usr/bin/env stack
-- stack --resolver lts-13.26 script
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.Async (concurrently)
import qualified Data.ByteString as B
import System.IO (hClose, stdout)
import System.Process.Typed

main :: IO ()
main = do
  let config = setStdin createPipe
             $ setStdout createPipe
             $ proc "cat" []
  ((), output) <- withProcess_ config $ \process -> concurrently
    (do B.hPut (getStdin process) "Hello World!\n"
        hClose (getStdin process))
    (do B.hGetContents (getStdout process))
  B.hPut stdout output