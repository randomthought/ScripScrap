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
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Control.Lens.TH (makeClassy, makeClassyPrisms)
import qualified Data.Set as S
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
import qualified Data.Text.IO as TIO
import System.IO
import GHC.Conc
import GHC.Conc.IO



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
  { selector :: SelectorProfile
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
  , _startingUrl :: String
  , _urlSplit :: UrlSplit
  , _selectors :: [SelectorProfile]
  , _excludePatterns :: [Pattern]
  , _includePatterns :: [Pattern]
  }
  deriving (Show, Eq)

instance FromJSON Target where
  parseJSON = withObject  "env" $ \m -> do
    targetName_ <- m .: "targetName"
    startingUrl_ <- m .: "startingUrl"
    let urlSplit_ = fromJust $ parseTLDText (T.pack startingUrl_)
    selectors_ <- m .: "selectors"
    excludedPatterns_ <- m .:? "excludePatterns" .!= []
    includedPatterns_ <- m .:? "includePatterns"  .!= []
    return $ Target targetName_ startingUrl_ urlSplit_ selectors_ excludedPatterns_ includedPatterns_

data Env = Env {
    _workers :: Int
  , _output :: FilePath
  , _targets :: [Target]
  }
  deriving Show
makeClassy ''Env

instance FromJSON Env where
  parseJSON = withObject  "env" $ \m -> Env
    <$> m .:? "workers" .!= 5
    <*> m .: "output"
    <*> m .: "targets"

data AppContext = AppContext {
    _apEnv :: Env
  , _apDb :: Handle
  , _apQueue :: !(TQueue QuedUrl)
  , _apProccessedUrls ::  !(TVar (S.Set Url))
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
  storeProcessed :: T.Text -> m ()

instance DataSource AppIO where
  storeToSource a = do
    path <- asks $ _output . _apEnv
    fh <- asks _apDb
    let str = BL.append (BL.fromStrict $ C8.pack "\n") (A.encode a)
    liftIO $ BL.hPut fh str

  notProcessed a = do
    mProcessed <- asks _apProccessedUrls
    processed <- liftIO $ atomically (readTVar mProcessed)
    return $ S.notMember a processed
  storeProcessed a = do
    mProcessed <- asks _apProccessedUrls
    liftIO $ atomically (modifyTVar mProcessed $ S.insert a)


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
  logMessage ::  String -> m ()

instance Logger AppIO where
  logMessage a = liftIO $ putStrLn a


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
    count <- asks _apWorkerCount
    liftIO $ atomically (modifyTVar count (1 -))
  currentWorkerCount = do
    count <- asks _apWorkerCount
    liftIO $ atomically (readTVar count)
