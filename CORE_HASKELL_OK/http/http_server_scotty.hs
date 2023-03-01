{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

main = scotty 8080 $ do
  get "/" $ do
    html $ "Hello world"