{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PolymorphicComponents #-}

-- python-interacttion.hs
-- import Distribution.Compat.TempFile

import Data.Set hiding (null)
import Data.List
import Data.Maybe
import Control.Monad

import Text.Printf
import Text.ParserCombinators.Parsec
import System.IO
import System.Process
import System.Directory
import System.FilePath
import System.Environment
-- import System.Posix.Temp
import System.IO.Temp
import Data.Foldable
import Data.Typeable
import Control.Applicative ((<$>), (<*>))

check :: (FilePath -> IO Bool) -> FilePath -> IO ()
check p s = do
  result <- p s
  putStrLn $
    s ++
    if result
      then do return True
      else do return False
	  
main :: IO ()
main = do 
-- [f,g] <- getArgs
    _tmpBaseDir <- getEnv "TMP"
    putStrLn _tmpBaseDir
    withTempDirectory _tmpBaseDir "sdist." $ \tmpDir -> do 
        putStrLn $ "Temporary file " ++ show _tmpBaseDir
        files <- readProcess "ls " [_tmpBaseDir] []
        for_ files $ \file -> do
            let ls = show file		 
            rt <- check doesFileExist ls
            if rt then do
                -- md <- readFile joinPath [_tmpBaseDir, file]
                putStrLn ls
            else
                putStrLn "No file"
    return 0