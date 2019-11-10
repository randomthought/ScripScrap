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

renderError :: AppError -> IO ()
renderError e = do
    putStrLn $ "There was an error:" ++ show e

makeAppConfigs :: Options -> IO (Either AppError AppContext)
makeAppConfigs (Options c o r) = do
  e <- decodeFileEither c :: IO (Either ParseException Env)
  p <- (getPermissions . takeDirectory) o
  let outPutRightable = writable p
  c <- case (e, outPutRightable) of
        (Right env, True) -> pure . Right =<< makeAppContext env
        (Left e, _) -> pure $ Left (IOError ("\n\tParse error: " ++ show e))
        (_, False) -> pure $ Left (IOError ("\n\tPermissions error: unable to write to" ++ show o))
  return c

restoreAppContext :: AppContext -> IO (Either AppContext Env)
restoreAppContext ac = error "Not Implemented"


makeAppContext :: Env -> IO AppContext
makeAppContext env = do
  apDb <- newTVarIO (_output env)
  apProccessedUrls <- newTVarIO S.empty :: IO (TVar (S.Set Url))
  let urlsQ = S.fromList $ map (\x -> (_targetId x, (T.pack . _startingUrl) x)) (_targets env)
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

--- e 1 -> 
