{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Lib
import Types
import Args

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (replicateM)
import Control.Monad.Loops
import qualified Data.Set as Set
import System.Environment
import Options.Applicative


main :: IO ()
main = do
  AppArgs (mode, conf) <- execParser opts
  urlsToProcess <- newTQueueIO :: IO (UrlsToProcess)
  _ <- populateQueue urlsToProcess mode
  let workerCount = workers conf
  currentWorkerCount <- newTVarIO workerCount :: IO (WorkerCount)
  processedHashes <- newTVarIO Set.empty :: IO (ProcessedHashes)
  replicateM workerCount $ forkIO $ do
    _ <- worker urlsToProcess processedHashes currentWorkerCount
    return ()

  whileM_ (f currentWorkerCount) $ threadDelay 1000000
    where f wc = do
            currentWorkerCount <- atomically $ readTVar wc
            let wait = if (currentWorkerCount > 0) then True else False
            return wait
