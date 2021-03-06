{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Lib (
  worker
  , createStartingQueue
) where

import Types

import Control.Concurrent
import Control.Lens.Getter
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans
import Data.Maybe
import Network.URI.TLD (parseTLDText)
import Text.Format
import Text.Regex.TDFA
import Text.XML.HXT.CSS
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Text.HandsomeSoup as HS
import qualified Text.Pretty.Simple as PS




worker :: (DataSource m
          , HasEnv t
          , Logger m
          , MonadIO m
          , MonadReader t m
          , Queue m
          , Requests m
          , WorkerState m) => m ()
worker = iterateUntilM lessThan5 work 0 >> pure ()
  where lessThan5 = (5 <)
        work n    = do linksFound <- whileJust pop processUrl
                       threadid   <- liftIO myThreadId
                       logMessage $ "Taking a break: " ++ (show threadid)
                       liftIO $ threadDelay 3000000
                       let n' = if null linksFound then (n + 1) else 0
                       _          <- if lessThan5 n' then decrimentWorkerCount else pure ()
                       return n'


processUrl :: (DataSource m
              , HasEnv t
              , Logger m
              , MonadIO m
              , MonadReader t m
              , Queue m
              , Requests m) => QuedUrl -> m ()
processUrl qUrl@(id, url) = do
  env <- ask
  (rc, body) <- send url
  let target = _target $ fromJust $ Map.lookup id (env ^. targets)
  let cssSelectors = _selectors $ target
  let url' = T.unpack url
  let containsContents = any (\p -> url' =~ p) (_extractPatterns target)
  let urlData = if containsContents then scrapeDocument cssSelectors body rc qUrl else Nothing
  -- PS.pPrint urlData
  maybe (pure ()) storeToSource urlData
  let links = map T.pack
              $ filter (\x -> all (not . (x =~)) (_excludePatterns target))
              $ filter (\x -> all (x =~) (_includePatterns target))
              $ map T.unpack
              $ urlCleanse (_urlSplit target)
              $ extractLinks body
  newUrls <-  filterProcessed links
  mapM_ (\url' -> push (id, url')) newUrls
  storeProcessed qUrl
  threadid <- liftIO myThreadId
  logMessage $ format "Url: {0}\nWith Thread: {1}\n" [show url, show threadid]

urlCleanse :: UrlSplit -> [Url] -> [Url]
urlCleanse uSpl@(_, domain, tld) urls
  = mapMaybe matchDomain fixedUrls
  where fixedUrls = map (fixUrlFormat uSpl)
                    $ filter (\x -> length (T.unpack x) > 0) urls
        matchDomain s = do (_, domain', tld') <- parseTLDText s
                           if (tld == tld') && (domain == domain')
                             then Just (s) else Nothing


fixUrlFormat :: UrlSplit -> Url -> Url
fixUrlFormat (subDom, domain, tld) url
  = let parsedUrl = format "https://{0}.{1}" [T.unpack domain, T.unpack tld]
        parseUrlWithWSubDomain = format "https://{0}.{1}.{2}" [T.unpack subDom, T.unpack domain, T.unpack tld]
        dom = if T.null subDom then parsedUrl else parseUrlWithWSubDomain
        url' = T.unpack url
    in case parseTLDText url of
         Just a ->  url
         Nothing -> T.pack $ dom ++ url'


linkIsEligable :: Domain -> [Pattern] -> Url -> Bool
linkIsEligable d ps url = error "Not implemented"

extractLinks :: PageData -> [Url]
extractLinks doc = map T.pack links
  where
    links = runLA (hread >>> HS.css "a" HS.! "href") doc


scrapeDocument :: [SelectorProfile] -> PageData -> ResponseCode -> QuedUrl -> Maybe UrlData
scrapeDocument ss doc rc (id, url) = let matches = filter (not . null . documents) $ map f ss
                                         f s = let selectors = _selector s
                                                   docs = map T.pack $ extractContent selectors doc
                                               in Matches { name = (_name s), documents = docs }
                                     in if null matches then Nothing
                                        else Just $ UrlData { url = url, matches = matches, targetName = id, responseCode = rc}


extractContent :: PageSelector -> PageData -> [String]
extractContent (Css s) doc   = runLA (hread >>> css s' //> getText) doc
  where s' = T.unpack s
extractContent (XPath s) doc = filter isEmptyString $ runLA (xshow trees) doc
  where s' = T.unpack s
        trees = hread >>> getXPathTrees s'
        isEmptyString = not . T.null . T.strip . T.pack


createStartingQueue :: Target -> [QuedUrl]
createStartingQueue t = map f (_startingUrls t)
  where f s = (target, T.pack s)
        target = (_targetName t)
