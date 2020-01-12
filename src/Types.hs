{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
-- |

module Types where


-- import Data.Aeson.Types (Parser, FromJSON, ToJSON)
import Control.Concurrent.STM
import Control.Concurrent.STM (TVar, TQueue)
import Control.Lens.TH (makeClassy, makeClassyPrisms)
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Aeson.Types
import Data.Aeson.Types (Parser)
import Data.Maybe
import Data.Yaml
import Data.Yaml.Config
import GHC.Conc
import GHC.Conc.IO
import GHC.Generics
import Network.Curl -- (curlGetString, curlGetResponse_, CurlOption(..) )
import Network.URI.TLD (parseTLDText)
import System.Directory (doesFileExist)
import System.IO
import qualified Control.Exception as E
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.BloomFilter as BF
import qualified Data.Map.Strict as SMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL



type Configs = FilePath

type OutPut = FilePath

type Resume = Bool

type Subdomain = T.Text

type TLD = T.Text

type UrlSplit = (Subdomain, Domain, TLD)

data Options = Options Configs Resume

type CssSelector = String

type XPathSelector = String

type PageData = String

type Links = Int

type Url = T.Text

type Response = (ResponseCode, String)

data PageSelector = XPath T.Text | Css T.Text
  deriving (Generic, Show, Eq)

data SelectorProfile = SelectorProfile {
    _name :: T.Text
  , _selector :: PageSelector
  }
  deriving (Generic, Show, Eq)

instance FromJSON SelectorProfile where
  parseJSON =
    let f t a = case t of
          "css"   -> return (Css a)
          "xpath" -> return (XPath a)
          _       -> fail $ "Unkown Selector type: " ++ t
    in withObject  "SelectorProfile" $ \m -> do
      name_ <- m .: "name"
      type_ <- m .: "type" :: Parser String
      selectorStr_ <- m .: "selector" :: Parser T.Text
      selector_ <- A.withText "selector"  (f type_) (String selectorStr_)
      return $ SelectorProfile name_ selector_


instance ToJSON SelectorProfile where
  toJSON selectorProfile = object [
        "name" .= _name selectorProfile
      , "selector" .= (show $ _selector selectorProfile)
    ]

data Matches = Matches
  { name :: T.Text
  , documents :: [T.Text]
  }
  deriving (Show, Eq)

instance ToJSON Matches where
  toJSON matches = object [
      "name" .= name matches
    , "documents" .= documents matches
    ]

instance FromJSON Matches where
  parseJSON = withObject "Matches" $ \o -> do
    name_ <- o .: "name"
    documents_ <- o .: "documents"
    return $ Matches name_ documents_

type ResponseCode = Int

data UrlData = UrlData {
    targetName :: TargetName
  , url :: Url
  , responseCode :: ResponseCode
  , matches :: [Matches]
  }
 deriving (Show, Eq, Generic)

instance A.ToJSON UrlData where
  toJSON urlData = object [
      "targetName" .= targetName urlData
    , "url" .= url urlData
    , "responseCode" .= responseCode urlData
    , "matches" .= matches urlData
    ]

instance FromJSON UrlData where
  parseJSON = withObject "UrlData" $ \o -> do
    targetName_ <- o .: "targetName"
    url_ <- o .: "url"
    responseCode_ <- o .: "responseCode"
    matches_ <- o .: "matches"
    return $ UrlData targetName_ url_ responseCode_ matches_

type Domain = T.Text

type Pattern = String

type TargetName = T.Text

type QuedUrl = (TargetName, Url)

type FileLock = TMVar Handle

data ScrapeTarget = ScrapeTarget
  {
    _target :: Target
  , _visitedHandle :: FileLock
  , _outPutHandle :: FileLock
  }

data Target = Target
  {
    _targetName :: TargetName
  , _startingUrls :: [String]
  , _urlSplit :: UrlSplit
  , _selectors :: [SelectorProfile]
  , _extractPatterns :: [Pattern]
  , _excludePatterns :: [Pattern]
  , _includePatterns :: [Pattern]
  , _output :: FilePath
  , _visited :: FilePath
  }
  deriving (Eq, Show)

instance FromJSON Target where
  parseJSON = withObject  "env" $ \m -> do
    targetName_ <- m .: "targetName"
    output_ <- m .: "output"
    visited_ <- m .: "visited"
    startingUrls_ <- m .: "startingUrls"
    let urlSplit_ = fromJust $ parseTLDText (T.pack $ head startingUrls_)
    selectors_ <- m .: "selectors"
    extractPatterns_ <- m .:? "extractPatterns" .!= []
    excludePatterns_ <- m .:? "excludePatterns" .!= []
    includePatterns_ <- m .:? "includePatterns" .!= []
    return $ Target
      targetName_
      startingUrls_
      urlSplit_
      selectors_
      extractPatterns_
      excludePatterns_
      includePatterns_
      output_
      visited_

getFileHandle :: FilePath -> IOMode -> IO Handle
getFileHandle fp mode = do
  exist <- doesFileExist fp
  fh <- if not exist then writeFile fp "" >> openFile fp mode else openFile fp mode
  return fh

data AppConfig = AppConfig {
    _appCorkers :: Int
  , _appCtargets :: [Target]
  }

instance FromJSON AppConfig where
  parseJSON = withObject  "env" $ \m -> AppConfig
    <$> m .:? "workers" .!= 5
    <*> m .: "targets"

data Env = Env {
    _workers :: Int
  , _targets :: SMap.Map TargetName ScrapeTarget
  }
makeClassy ''Env

data AppContext = AppContext {
    _apEnv :: !Env
  , _apQueue :: !(TQueue QuedUrl)
  , _apProccessedUrls ::  !(TVar (BF.Bloom String))
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
  -- output = apEnv . output
  workers = apEnv . workers
  targets = apEnv . targets

class Monad m => DataSource m where
  storeToSource :: UrlData -> m ()
  storeProcessed :: QuedUrl -> m ()
  notProcessed :: T.Text -> m Bool
  filterProcessed :: [T.Text] -> m [T.Text]

instance DataSource AppIO where
  storeToSource a = do
    env <- asks _apEnv
    let fileLock = _outPutHandle $ fromJust $ SMap.lookup (targetName a) (_targets env)
    fh <- liftIO $ atomically (takeTMVar fileLock)
    let encoded = TL.toStrict $ A.encodeToLazyText a
    liftIO $ T.hPutStrLn fh encoded
    liftIO $ atomically (putTMVar fileLock fh)
  storeProcessed (k, url) = do
    env <- asks _apEnv
    let fileLock = _visitedHandle $ fromJust $ SMap.lookup k (_targets env)
    fh <- liftIO $ atomically (takeTMVar fileLock)
    let url' = T.unpack url
    liftIO $ hPutStrLn fh url'
    liftIO $ atomically (putTMVar fileLock fh)
  notProcessed a = do
    mProcessed <- asks _apProccessedUrls
    processed <- liftIO $ atomically (readTVar mProcessed)
    let a' = T.unpack a
    return $ BF.notElem a' processed
  filterProcessed as = do
    mProcessed <- asks _apProccessedUrls
    processed <- liftIO $ atomically (readTVar mProcessed)
    let as' = map T.unpack as
    let np = map T.pack $ filter (\x -> BF.notElem x processed) as'
    return np


class Monad m => Queue m where
  push :: QuedUrl -> m ()
  pop :: m (Maybe QuedUrl)

instance Queue AppIO where
  pop = do
    queue <- asks _apQueue
    liftIO $ atomically (tryReadTQueue queue)
  push a@(id, url) = do
    queue <- asks _apQueue
    mProcessed <- asks _apProccessedUrls
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
