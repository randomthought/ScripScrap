{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Lib (
  UrlsToProcess
, WorkerCount
, ProcessedHashes
, worker
) where

import Prelude hiding (putStrLn)
import Data.String.Class (putStrLn)
import Data.Text.Internal (showText)
import Control.Concurrent
import Control.Concurrent.STM
import Network.Curl
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Loops
import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import Text.XML.HXT.TagSoup
import Text.HandsomeSoup

type ProcessedHashes = TVar (Set.Set String)

type UrlsToProcess = TQueue String

type Links = Int

type WorkerCount = TVar Int

data UrlData = UrlData {hash :: String, url :: URLString}
  deriving (Show, Eq)

-- stringToHash :: String -> UrlData
-- stringToHash =  error "Not implemented"

-- writeResults :: UrlData -> FilePath -> IO ()
-- writeResults _ _ = putStrLn "Do"

worker :: UrlsToProcess -> ProcessedHashes -> WorkerCount -> IO Int
worker queue processed workerCount =
  let lessThan5      = (5 <)
      decrimentWorkers = do
          _ <- atomically $ modifyTVar' workerCount (\n -> n - 1)
          return ()
      work n           = do
        linksFound <- whileJust (atomically $ tryReadTQueue queue) (processUrl processed)
        threadid   <- myThreadId
        putStrLn $ "Taking a break: " ++ (show threadid)
        threadDelay 3000000
        let n' = if (null linksFound) then (n + 1) else 0
        _          <- if (lessThan5 n') then decrimentWorkers else pure ()
        return n'
  in iterateUntilM lessThan5 work 0

processUrl:: ProcessedHashes -> URLString -> IO Links
processUrl _ url = do
  let curlOptions = [CurlTimeout 3, CurlFollowLocation True, CurlMaxRedirs 5]
  (code, body) <- curlGetString url curlOptions
  l <- extractLinks body
  print l
  threadid <- myThreadId
  putStrLn $ "Url: " ++ url ++ "\nResponse: " ++ show code ++ "\nWith Thread: " ++ show threadid ++ "\n"
  return (0 :: Links)

type Selector = String
type PageData = String

extractContents :: String -> [Selector] -> [PageData]
extractContents body s =  error "Need to implement"

-- extractLinks :: String -> [URLString]
-- extractLinks c = error "Implement"

extractLinks :: String -> IO [URLString]
extractLinks body = extract (readString
  [ withParseHTML yes, withWarnings no
  ] body)

extract doc = runX $ doc //> hasName "a" >>> getAttrValue "href"
-- extract doc = runX $ doc >>> xmlFilter "article" >>> xmlFilter "a" >>> toHref

-- toHref = proc el -> do
--    link    <- getAttrValue "href" -< el
--    returnA -< link
