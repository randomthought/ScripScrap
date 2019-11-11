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
import qualified Data.ByteString.Char8 as C
import Control.Lens.TH (makeClassy, makeClassyPrisms)
import qualified Data.Set as S
import Data.Yaml
import Data.Yaml.Config
import Data.Aeson (withObject)
import Control.Monad.Except
import qualified Control.Exception as E
import Control.Monad.IO.Unlift
import Data.Maybe
import Network.URI.TLD (parseTLDText)
import Data.Aeson.Types (Parser)
import GHC.Generics




type Configs = FilePath
type OutPut = FilePath
type Resume = Bool

type Subdomain = T.Text

type TLD = T.Text

type UrlSplit = (Subdomain, Domain, TLD)

data Options = Options Configs OutPut Resume

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
    targetId :: Int
  , url :: T.Text
  , responseCode :: ResponseCode
  , matches :: [Matches]
  }
 deriving (Show, Eq)

instance ToJSON UrlData where
  toJSON urlData = object [
      "targetId" .= targetId urlData
    , "url" .= url urlData
    , "responseCode" .= responseCode urlData
    , "matches" .= matches urlData
    ]


type Domain = T.Text

type Pattern = String

type TargetId = Int

type QuedUrl = (TargetId, Url)


data Target = Target
  {
    _targetId :: TargetId
  , _startingUrl :: String
  , _urlSplit :: UrlSplit
  , _selectors :: [SelectorProfile]
  , _excludePatterns :: [Pattern]
  , _includePatterns :: [Pattern]
  }
  deriving (Show, Eq)

instance FromJSON Target where
  parseJSON = withObject  "env" $ \m -> do
    targetId_ <- m .: "targetId"
    startingUrl_ <- m .: "startingUrl"
    let urlSplit_ = fromJust $ parseTLDText (T.pack startingUrl_)
    selectors_ <- m .: "selectors"
    excludedPatterns_ <- m .:? "excludePatterns" .!= []
    includedPatterns_ <- m .:? "includePatterns"  .!= []
    return $ Target targetId_ startingUrl_ urlSplit_ selectors_ excludedPatterns_ includedPatterns_

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
  , _apDb :: !(TVar FilePath)
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

-- instance HasTarget AppContext where
--   targetId = apEnv .

class Monad m => DataSource m where
  storeToSource :: UrlData -> m ()
  notProcessed :: T.Text -> m Bool
  storeProcessed :: T.Text -> m ()

instance DataSource AppIO where
  storeToSource a = do
    mPath <- asks _apDb
    liftIO $ C.putStrLn (encode a)
    -- path <- liftIO $ atomically (readTVar mPath)
    -- liftIO $ writeFile path (show a)
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
  logMessage :: Show a => a -> m ()

instance Logger AppIO where
  logMessage a = liftIO $ putStrLn $ show a


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
