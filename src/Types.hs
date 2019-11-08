{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Types where


import Control.Concurrent.STM (TVar, TQueue)
import Network.Curl -- (curlGetString, curlGetResponse_, CurlOption(..) )
import Control.Monad.Reader
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.BloomFilter as B
import Control.Lens.TH (makeClassy, makeClassyPrisms)

type CssSelector = T.Text

type PageData = String

type Links = Int

type Url = T.Text

type WorkerCount = TVar Int

data Selector = Match {
    _selector :: CssSelector
  , _name :: T.Text
  }
  deriving (Show, Eq)

data Matches = Matches
  { selector :: Selector
  , name :: T.Text
  , documents :: [T.Text]
  }
  deriving (Show, Eq)

type ResponseCode = Int

data UrlData = UrlData {
    url :: T.Text
  , responseCode :: ResponseCode
  , targetId :: Int
  , matches :: [Matches]
  }
 deriving (Show, Eq)

type Domain = T.Text

type Pattern = String

type TargetId = Int

type QuedUrl = (TargetId, Url)


data Target = Target
  {
    _targetId :: TargetId
  , _startingUrl :: String
  , _domain :: Domain
  , _selectors :: [Selector]
  , _excludePatterns :: [Pattern]
  , _includePatterns :: [Pattern]
  }
  deriving (Show, Eq)
-- makeClassy ''Target

data Env = Env {
    _workers :: Int
  , _output :: FilePath
  , _targets :: [Target]
  }
makeClassy ''Env

data AppContext = AppContext {
    _apEnv :: Env
  , _apDb :: !(TVar FilePath)
  , _apQueue :: !(TQueue QuedUrl)
  , _apProccessedUrls ::  !(TVar (B.Bloom T.Text))
  , workerCount :: !(TVar Int)
  }
makeClassy ''AppContext

newtype AppIO a =
  AppIO { unAppIO :: ReaderT AppContext IO a }
  deriving (Functor, Applicative, Monad, MonadReader AppContext, MonadIO) -- MonadUnliftIO)

instance HasEnv AppContext where
  output = apEnv . output
  workers = apEnv . workers
  targets = apEnv . targets

-- instance HasTarget AppContext where
--   targetId = apEnv .

class Monad m => DataSource m where
  storeToSource :: UrlData -> m ()
  notInSource :: T.Text -> m Bool

instance DataSource AppIO where
  storeToSource a = do
    mPath <- asks _apDb
    path <- liftIO $ atomically (readTVar mPath)
    liftIO $ writeFile path (show a)
  notInSource a = do
    mProcessed <- asks _apProccessedUrls
    processed <- liftIO $ atomically (readTVar mProcessed)
    return $ B.notElem a processed


class Monad m => Queue m where
  push :: QuedUrl -> m ()
  pop :: m (Maybe QuedUrl)

instance Queue AppIO where
  pop = do
    queue <- asks _apQueue
    liftIO $ atomically (tryReadTQueue queue)
  push a = do
    queue <- asks _apQueue
    liftIO $ atomically (writeTQueue queue a)

class Monad m => Logger m where
  logMessage :: Show a => a -> m ()

instance Logger AppIO where
  logMessage a = liftIO $ print a


type Response = (ResponseCode, String)

class Monad m => Requests m where
  send :: Url -> m Response

instance Requests AppIO where
  send url = do
    let curlOptions = [CurlTimeout 3, CurlFollowLocation True, CurlMaxRedirs 2]
    resp <- liftIO $ curlGetResponse_ (T.unpack url) curlOptions :: AppIO CurlResponse
    let code = respStatus resp
    let body = respBody resp
    return (code, body)


class (Monad m) => WorkerState m where
  decrimentWorkerCount :: m ()
  currentWorkerCount :: m Int

instance WorkerState AppIO where
  decrimentWorkerCount = do
    count <- asks workerCount
    liftIO $ atomically (modifyTVar count (1 -))
  currentWorkerCount = do
    count <- asks workerCount
    liftIO $ atomically (readTVar count)
