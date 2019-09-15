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
import Text.HandsomeSoup
import Text.Pretty.Simple (pPrint)


worker :: UrlsToProcess -> ProcessedHashes -> WorkerCount -> IO Int
worker queue processed workerCount =
  let lessThan5        = (5 <)
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
  let curlOptions = [CurlTimeout 3, CurlFollowLocation True, CurlMaxRedirs 2]
  (code, body) <- curlGetString url curlOptions
  let urlData = scrapeDocument body [".storylink", ".sitestr"] url
  pPrint urlData
  threadid <- myThreadId
  putStrLn $ "Url: " ++ url ++ "\nResponse: " ++ show code ++ "\nWith Thread: " ++ show threadid ++ "\n"
  return (0 :: Links)

extractLinks :: PageData -> [URLString]
extractLinks doc = runLA (hread >>> css "a" ! "href") doc

scrapeDocument :: PageData -> [Selector] -> URLString -> UrlData
scrapeDocument doc ss u = UrlData { url = u , matches = ms}
  where ms  = map f ss
        f s = let docs = extractContent doc s
          in Matches { selector = s, name = "TODO", documents = docs }
        

extractContent :: PageData -> Selector -> [String]
extractContent doc s = runLA (hread >>> css s //> getText) doc

populateQueue :: UrlsToProcess -> ScrapeMode -> IO ()
populateQueue queue (NewScrape url) = atomically $ writeTQueue queue url
populateQueue queue (Targeted file) = do
  contents <- readFile file
  let linesOfFile = lines contents
  mapM_ (atomically . writeTQueue queue) linesOfFile
