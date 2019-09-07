{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (replicateM)
import Control.Monad.Loops
import qualified Data.Set as Set

main :: IO ()
main = do
  let workerCount = 3
  workerCount' <- newTVarIO workerCount :: IO (WorkerCount)
  urlsToProcess <- newTQueueIO :: IO (UrlsToProcess)
  processedHashes <- newTVarIO Set.empty :: IO (ProcessedHashes)
  replicateM workerCount $ forkIO $ do
    _ <- worker urlsToProcess processedHashes workerCount'
    return ()

  whileM_ (f workerCount') $ threadDelay 1000000
    where f wc = do
            currentWorkerCount <- atomically $ readTVar wc
            let wait = if (currentWorkerCount > 0) then True else False
            return wait

