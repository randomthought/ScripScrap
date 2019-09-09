{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Lib (
  worker
, populateQueue
) where

import Types

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

extractContents :: String -> [Selector] -> [PageData]
extractContents body s =  error "Need to implement"

extractLinks :: String -> IO [URLString]
extractLinks body = extract (readString
  [ withParseHTML yes, withWarnings no
  ] body)
  where extract doc =  runX $ doc //> hasName "a" >>> getAttrValue "href"

populateQueue :: UrlsToProcess -> ScrapeMode -> IO ()
populateQueue queue (NewScrape url) = atomically $ writeTQueue queue url
populateQueue queue (Targeted file) = do
  contents <- readFile file
  let linesOfFile = lines contents
  mapM_ (atomically . writeTQueue queue) linesOfFile
