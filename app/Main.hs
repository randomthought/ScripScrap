{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Lib
import Types
import Args

import Control.Monad (replicateM)
import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.Reader
import Data.BloomFilter.Hash (hashes)
import Data.Either
import Data.Maybe (catMaybes)
import Data.Yaml
import Options.Applicative
import System.Directory (doesFileExist)
import System.EasyFile -- (getPermissions, takeDirectorym, Permissions(..))
import System.Environment
import System.IO
import UnliftIO.Concurrent (forkIO, threadDelay)
import UnliftIO.STM
import qualified Control.Exception as E
import qualified Data.Aeson as A
import qualified Data.BloomFilter as BF
import qualified Data.BloomFilter.Mutable as BFM
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Set as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Map.Strict as SMap

renderError :: AppError -> IO ()
renderError e = do
    putStr $ "There was an error:" ++ show e

makeAppConfigs :: Options -> IO (Either AppError AppContext)
makeAppConfigs (Options c r) = do
  e <- decodeFileEither c :: IO (Either ParseException AppConfig)
  c <- case e of
        (Right env) -> pure . Right =<< makeAppContext env
        (Left err ) -> pure $ Left (IOError ("\n\tParse error: " ++ show err))
  return c

makeAppContext :: AppConfig -> IO AppContext
makeAppContext appConf = do
  let targets = _appCtargets appConf
  let visitedPaths = map _visited targets
  bloomFilter <- populateVisited visitedPaths
  processedUrls <- newTVarIO bloomFilter :: IO (TVar (BF.Bloom String))
  apWorkerCount <- newTVarIO (_appCorkers appConf) :: IO (TVar Int)
  env <- makeAppEnv appConf
  let urlsQ = S.fromList $ concat $ map createStartingQueue targets
  apQueue <- newTQueueIO :: IO (TQueue QuedUrl)
  atomically $ mapM_ (writeTQueue apQueue) urlsQ
  return AppContext {
    _apEnv = env
  , _apQueue =  apQueue
  , _apProccessedUrls = processedUrls
  , _apWorkerCount = apWorkerCount
    }

makeAppEnv :: AppConfig -> IO Env
makeAppEnv conf = do
  st <- mapM makeScrapeTarget $ _appCtargets conf
  let f = _targetName . _target
  let stMap = SMap.fromList $ map (\x -> (f x, x)) st
  return $ Env (_appCorkers conf) stMap

makeScrapeTarget :: Target -> IO ScrapeTarget
makeScrapeTarget target = do
  oFh <- openFile (_output target) AppendMode
  oFl <- newTMVarIO oFh
  vFh <- openFile (_visited target) AppendMode
  vFl <- newTMVarIO vFh
  return $ ScrapeTarget target vFl oFl

populateVisited :: [FilePath] -> IO (BF.Bloom String)
populateVisited filePaths = do
  let bf = BF.empty (hashes 16) 47925292 :: BF.Bloom String
  contents <- mapM readFile filePaths
  let urls = join $ map lines contents
  let !bf' = BF.insertList urls bf
  return bf'

runProgram :: AppContext -> IO ()
runProgram ac = runReaderT (unAppIO run) ac


run :: AppIO ()
run = do
  reqWorkerCount <- asks (_workers . _apEnv)
  workerCount <- asks _apWorkerCount
  replicateM reqWorkerCount (forkIO worker)
  whileM_ (f workerCount) $ threadDelay 1000000
    where f wc' = do
            c <- atomically $ readTVar wc'
            let wait = if (c > 0) then True else False
            return wait

main :: IO ()
main = either renderError runProgram
     =<< makeAppConfigs
     =<< execParser opts
