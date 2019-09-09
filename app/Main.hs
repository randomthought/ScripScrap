{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Lib

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (replicateM)
import Control.Monad.Loops
import qualified Data.Set as Set
import System.Environment
import Options.Applicative
import Data.Semigroup ((<>))

data ScrapeConfig = ScrapeConfig {
   cssSelectors :: [String]
  , excudePatterns :: [String]
  , workers :: Int
  , output :: FilePath
  }
  deriving (Show)

data ScrapeMode = NewScrape String | Targeted [String]
  deriving (Show)

data AppArgs = AppArgs (ScrapeMode, ScrapeConfig)
  deriving (Show)

confParser :: Parser ScrapeConfig
confParser = ScrapeConfig
  <$> some (option auto (long "css"
                         <> short 'c'
                         <> metavar "SELECTOR"
                         <> help "A list of CSS Selectors you wish to extract from each url"))
  <*> some (option auto (long "regex"
                         <> short 'r'
                         <> metavar "PATTERN"
                         <> help "A list of regex patterns you wish to filter out discovered urls you don't wish to scrape"))
  <*> option auto (long "workers"
                   <> short 'w'
                   <> value 3
                   <> metavar "INT"
                   <> help "Number of worker threads you wish to use")
  <*> strOption (long "output"
                   <> short 'o'
                   <> metavar "FILEDIR"
                   <> help "Location where you wish to store the scrapped contents")

newScrapeParser = NewScrape
  <$> strOption (long "url"
                 <> short 'u'
                   <> metavar "URL"
                 <> help "The url to scrape")

targetedScrapeParser = Targeted
  <$> some (option auto (long "file"
                         <> short 'f'
                         <> metavar "FILEDIR"
                         <> help "Text file containing the list of urls you want to scrape"))

modeScrapeParser = newScrapeParser <|> targetedScrapeParser

appArgsParser = AppArgs <$> liftA2 (,) modeScrapeParser confParser

opts = info (appArgsParser <**> helper) fullDesc


main :: IO ()
main = do
  options <- execParser opts
  print options

  
  -- arg <- cmdArgs newScrape
  -- print arg
  -- urlsToProcess <- newTQueueIO :: IO (UrlsToProcess)
  -- contents <- readFile $ output arg
  -- let linesOfFile = lines contents
  -- mapM_ (atomically . writeTQueue urlsToProcess) (take 10 linesOfFile)
  -- _ <- atomically $ writeTQueue urlsToProcess (url arg)
  -- workerCount' <- newTVarIO (workers arg) :: IO (WorkerCount)
  -- processedHashes <- newTVarIO Set.empty :: IO (ProcessedHashes)
  -- replicateM (workers arg) $ forkIO $ do
  --   _ <- worker urlsToProcess processedHashes workerCount'
  --   return ()

  -- whileM_ (f workerCount') $ threadDelay 1000000
  --   where f wc = do
  --           currentWorkerCount <- atomically $ readTVar wc
  --           let wait = if (currentWorkerCount > 0) then True else False
  --           return wait
