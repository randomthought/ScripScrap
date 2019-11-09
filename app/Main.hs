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
import qualified Data.Text as T
import qualified Data.BloomFilter as B
import Data.BloomFilter.Hash (cheapHashes)
import qualified Data.Set as S
-- import Data.Vector (Vector)
import Data.Yaml

newScrape :: FilePath -> IO ()
newScrape file = do
  r <- decodeFileEither file :: IO (Either ParseException Env)
  print r


main :: IO ()
main = do
  scrapeMode <- execParser opts
  case scrapeMode of
    New filePath -> newScrape filePath
    Resume filePath -> newScrape filePath
  -- urlsToProcess <- newTQueueIO :: IO (TQueue QuedUrl)
  -- proccessedUrls <- newTVarIO S.empty :: IO (TVar (S.Set Url))

  -- _ <- populateQueue urlsToProcess mode
  -- let workerCount = workers conf
  -- currentWorkerCount <- newTVarIO workerCount :: IO (WorkerCount)
  -- processedHashes <- newTVarIO Set.empty :: IO (ProcessedHashes)
  -- replicateM workerCount $ forkIO $ do
  --   _ <- worker urlsToProcess processedHashes currentWorkerCount
  --   return ()

  -- whileM_ (f currentWorkerCount) $ threadDelay 1000000
  --   where f wc = do
  --           currentWorkerCount <- atomically $ readTVar wc
  --           let wait = if (currentWorkerCount > 0) then True else False
  --           return wait
