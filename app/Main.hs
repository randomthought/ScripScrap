{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Lib
import Types
import Args

import UnliftIO.Concurrent (forkIO, threadDelay)
import UnliftIO.STM
import Control.Monad (replicateM)
import Control.Monad.Loops
import qualified Data.Set as Set
import System.Environment
import Options.Applicative
import qualified Data.Text as T
import qualified Data.Set as S
import Control.Monad.Reader
import Data.Yaml
import Data.Either
import Control.Monad.Except
import qualified Control.Exception as E
import System.EasyFile -- (getPermissions, takeDirectorym, Permissions(..))
import System.IO

renderError :: AppError -> IO ()
renderError e = do
    putStrLn $ "There was an error:" ++ show e

makeAppConfigs :: Options -> IO (Either AppError AppContext)
makeAppConfigs (Options c r) = do
  e <- decodeFileEither c :: IO (Either ParseException Env)
  c <- case e of
        (Right env) -> pure . Right =<< makeAppContext env
        (Left e ) -> pure $ Left (IOError ("\n\tParse error: " ++ show e))
  return c

restoreAppContext :: AppContext -> IO (Either AppContext Env)
restoreAppContext ac = error "Not Implemented"


makeAppContext :: Env -> IO AppContext
makeAppContext env = do
  apDb <- openFile (_output env) AppendMode
  apProccessedUrls <- newTVarIO S.empty :: IO (TVar (S.Set Url))
  let urlsQ = S.fromList $ map (\x -> (_targetName x, (T.pack . _startingUrl) x)) (_targets env)
  apQueue <- newTQueueIO :: IO (TQueue QuedUrl)
  atomically $ mapM_ (writeTQueue apQueue) urlsQ
  apWorkerCount <- newTVarIO (_workers env) :: IO (TVar Int)
  return AppContext {
    _apEnv = env
  , _apDb  = apDb
  , _apQueue =  apQueue
  , _apProccessedUrls = apProccessedUrls
  , _apWorkerCount = apWorkerCount
    }

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
