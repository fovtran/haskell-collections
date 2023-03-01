#!/usr/bin/env runhaskell
{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, OverloadedStrings #-}

import Shelly
import Prelude hiding (FilePath)
import Text.Shakespeare.Text (lt)
import qualified Data.Text.Lazy as LT
import Control.Monad (forM)
import System.Environment (getArgs)

import qualified Control.Concurrent.QSem as QSem
import Control.Concurrent (forkIO, MVar, putMVar, newEmptyMVar, takeMVar)

-- Define max number of simultaneous processes
maxProcesses :: IO QSem.QSem
maxProcesses = QSem.newQSem 1

bkGrnd :: ShIO a -> ShIO (MVar a)
bkGrnd proc = do
  mvar <- liftIO newEmptyMVar
  _ <- liftIO $ forkIO $ do
    -- Block until there are free processes
    sem <- maxProcesses
    QSem.waitQSem sem
    putStrLn "Starting"
    -- Run the shell command
    result <- shelly $ silently proc
    liftIO $ putMVar mvar result
    putStrLn "Done"
    -- Signal that this process is done and another can run.
    QSem.signalQSem sem
  return mvar

main :: IO ()
main = shelly $ silently $ do
    [img, file] <- liftIO $ getArgs
    contents <- readfile $ fromText $ LT.pack file
    -- Run a backgrounded process for each line of input.
    results <- forM (LT.lines contents) $ \line -> bkGrnd $ do
      runStdin <command> <arguments>
    liftIO $ mapM_ takeMVar results
	
	
bkGrnd :: QSem.QSem -> ShIO a -> ShIO (MVar a)
bkGrnd sem proc = do
  mvar <- liftIO newEmptyMVar
  _ <- liftIO $ forkIO $ do
    -- Block until there are free processes
    QSem.waitQSem sem
    --
    -- code continues as before
    --

main :: IO ()
main = shelly $ silently $ do
    [img, file] <- liftIO $ getArgs
    contents <- readfile $ fromText $ LT.pack file
    sem <- maxProcesses
    -- Run a backgrounded process for each line of input.
    results <- forM (LT.lines contents) $ \line -> bkGrnd sem $ do
      runStdin <command> <arguments>
    liftIO $ mapM_ takeMVar results
	
Isn't it better

bkGrnd sem proc = do
  QSem.waitQSem sem
  mvar <- liftIO newEmptyMVar
  _ <- liftIO $ forkIO $ do
  ...