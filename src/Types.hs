{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
-- |

module Types where


import Control.Concurrent.STM (TVar, TQueue)
import Network.Curl -- (curlGetString, curlGetResponse_, CurlOption(..) )
import Control.Monad.Reader
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Lens.TH (makeClassy, makeClassyPrisms)
import Data.Yaml
import Data.Yaml.Config
import qualified Data.Aeson as A
import Control.Monad.Except
import qualified Control.Exception as E
import Control.Monad.IO.Unlift
import Data.Maybe
import Network.URI.TLD (parseTLDText)
import Data.Aeson.Types (Parser)
import GHC.Generics
import System.IO
import GHC.Conc
import GHC.Conc.IO
import qualified Data.BloomFilter as BF



type Configs = FilePath

type OutPut = FilePath

type Resume = Bool

type Subdomain = T.Text

type TLD = T.Text

type UrlSplit = (Subdomain, Domain, TLD)

data Options = Options Configs Resume

type CssSelector = String

type PageData = String

type Links = Int

type Url = T.Text

type Response = (ResponseCode, String)

data SelectorProfile = Selector {
    _name :: T.Text
  , _selector :: CssSelector
  }
  deriving (Generic, Show, Eq)

instance FromJSON SelectorProfile where
  parseJSON = withObject  "Selector" $ \m -> Selector
    <$> m .: "name"
    <*> m .: "selector"

instance ToJSON SelectorProfile where
  toJSON selectorProfile = object [
        "name" .= _name selectorProfile
      , "selector" .= _selector selectorProfile
    ]

data Matches = Matches
  { selector :: CssSelector
  , name :: T.Text
  , documents :: [T.Text]
  }
  deriving (Show, Eq)

instance ToJSON Matches where
  toJSON matches = object [
      "name" .= name matches
    , "selector" .= selector matches
    , "documents" .= documents matches
    ]

type ResponseCode = Int

data UrlData = UrlData {
    targetName :: TargetName
  , url :: Url
  , responseCode :: ResponseCode
  , matches :: [Matches]
  }
 deriving (Show, Eq)

instance A.ToJSON UrlData where
  toJSON urlData = object [
      "targetName" .= targetName urlData
    , "url" .= url urlData
    , "responseCode" .= responseCode urlData
    , "matches" .= matches urlData
    ]


type Domain = T.Text

type Pattern = String

type TargetName = T.Text

type QuedUrl = (TargetName, Url)


data Target = Target
  {
    _targetName :: TargetName
  , _startingUrls :: [String]
  , _urlSplit :: UrlSplit
  , _selectors :: [SelectorProfile]
  , _extractPatterns :: [Pattern]
  , _excludePatterns :: [Pattern]
  , _includePatterns :: [Pattern]
  }
  deriving (Show, Eq)

instance FromJSON Target where
  parseJSON = withObject  "env" $ \m -> do
    targetName_ <- m .: "targetName"
    startingUrls_ <- m .: "startingUrls"
    let urlSplit_ = fromJust $ parseTLDText (T.pack $ head startingUrls_)
    selectors_ <- m .: "selectors"
    extractPatterns_ <- m .:? "extractPatterns" .!= []
    excludePatterns_ <- m .:? "excludePatterns" .!= []
    includePatterns_ <- m .:? "includePatterns"  .!= []
    return $ Target targetName_ startingUrls_ urlSplit_ selectors_ extractPatterns_ excludePatterns_ includePatterns_

data Env = Env {
    _workers :: Int
  , _output :: FilePath
  , _visited :: FilePath
  , _targets :: [Target]
  }
  deriving Show
makeClassy ''Env

instance FromJSON Env where
  parseJSON = withObject  "env" $ \m -> Env
    <$> m .:? "workers" .!= 5
    <*> m .: "output"
    <*> m .: "visited"
    <*> m .: "targets"

type FileLock = TMVar Handle

data AppContext = AppContext {
    _apEnv :: !Env
  , _apDb :: !FileLock
  , _apQueue :: !(TQueue QuedUrl)
  , _apProccessedUrls ::  !(TVar (BF.Bloom String), FileLock)
  , _apWorkerCount :: !(TVar Int)
  }
makeClassy ''AppContext


data AppError = IOError String
  deriving Show


newtype AppIO a =
  -- AppIO { unAppIO :: ReaderT AppContext (ExceptT AppError IO) a }
  AppIO { unAppIO :: ReaderT AppContext IO a }
  deriving (Functor
           , Applicative
           , Monad
           , MonadReader AppContext
           , MonadIO
           , MonadUnliftIO
           -- , MonadError AppError
           )
           -- , MonadBaseControl IO) -- MonadUnliftIO)

instance HasEnv AppContext where
  output = apEnv . output
  workers = apEnv . workers
  targets = apEnv . targets

class Monad m => DataSource m where
  storeToSource :: UrlData -> m ()
  notProcessed :: T.Text -> m Bool
  filterProcessed :: [T.Text] -> m [T.Text]
  storeProcessed :: T.Text -> m ()

instance DataSource AppIO where
  storeToSource a = do
    fileLock <- asks _apDb
    fh <- liftIO $ atomically (takeTMVar fileLock)
    liftIO $ hPutStrLn fh (show a)
    liftIO $ atomically (putTMVar fileLock fh)
  notProcessed a = do
    (mProcessed, _) <- asks _apProccessedUrls
    processed <- liftIO $ atomically (readTVar mProcessed)
    let a' = T.unpack a
    return $ BF.notElem a' processed
  filterProcessed as = do
    (mProcessed, _) <- asks _apProccessedUrls
    processed <- liftIO $ atomically (readTVar mProcessed)
    let as' = map T.unpack as
    let np = map T.pack $ filter (\x -> BF.notElem x processed) as'
    return np

  storeProcessed a = do
    let a' = T.unpack a
    (_, fileLock) <- asks _apProccessedUrls
    fh <- liftIO $ atomically (takeTMVar fileLock)
    liftIO $ hPutStrLn fh a'
    liftIO $ atomically (putTMVar fileLock fh)


class Monad m => Queue m where
  push :: QuedUrl -> m ()
  pop :: m (Maybe QuedUrl)

instance Queue AppIO where
  pop = do
    queue <- asks _apQueue
    liftIO $ atomically (tryReadTQueue queue)
  push a@(id, url) = do
    queue <- asks _apQueue
    (mProcessed, _) <- asks _apProccessedUrls
    bf <- liftIO $ readTVarIO mProcessed
    let url' = T.unpack url
    liftIO$ if BF.elem url' bf then pure () else
      atomically (modifyTVar mProcessed $ BF.insert url') >> atomically (writeTQueue queue a)


class Monad m => Logger m where
  logMessage ::  String -> m ()

instance Logger AppIO where
  logMessage a = liftIO $ putStrLn a

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
    count <- asks _apWorkerCount
    liftIO $ atomically (modifyTVar count (1 -))
  currentWorkerCount = do
    count <- asks _apWorkerCount
    liftIO $ atomically (readTVar count)
