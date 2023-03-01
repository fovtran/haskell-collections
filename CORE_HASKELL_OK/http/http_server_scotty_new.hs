{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.Lazy (pack)
import System.Environment (getEnv)
import Web.Scotty (get, html, scotty)

envVar = getEnv "MY_VAR"

main = do
  myVar <- envVar
  scotty 8080 $ do
    get "/" $ do
      html . pack $ "Hello world " ++ myVar
