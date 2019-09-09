-- | 

module Types where


import Control.Concurrent.STM (TVar, TQueue)
import Network.Curl (URLString)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.XML.HXT.Curl

type Selector = String

type PageData = String

type ProcessedHashes = TVar (Set.Set String)

type UrlsToProcess = TQueue String

type Links = Int

type WorkerCount = TVar Int

data UrlData = UrlData {hash :: String, url :: URLString}
  deriving (Show, Eq)

data ScrapeConfig = ScrapeConfig {
   cssSelectors :: [String]
  , excudePatterns :: [String]
  , workers :: Int
  , output :: FilePath
  }
  deriving (Show)

data ScrapeMode = NewScrape String | Targeted String
  deriving (Show)

data AppArgs = AppArgs (ScrapeMode, ScrapeConfig)
  deriving (Show)
