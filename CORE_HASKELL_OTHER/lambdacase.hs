{-# LANGUAGE LambdaCase #-}

import System.Directory

main = do
    doesFileExist "/etc/passwd" >>= \case
        True ->  putStrLn "Yes"
        False -> putStrLn "No"