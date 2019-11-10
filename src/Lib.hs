{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Lib (
  worker
) where

import Types

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens.Getter (view)
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans
-- import Data.String.Class (putStrLn)
import Data.Text.Internal (showText)
-- import Prelude hiding (putStrLn)
import Text.HandsomeSoup
import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import qualified Data.Text as T
import Control.Monad.Reader.Class (MonadReader)
import Control.Lens.Getter
import Control.Monad.Reader



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
  let target = getTarget id (env ^. targets)
  let css = _selectors $ target
  let urlData = scrapeDocument css body rc qUrl
  maybe (pure ()) storeToSource urlData
  let patterns = _excludePatterns target
  let d = _domain target
  let links = filter (linkIsEligable d patterns) $ extractLinks body
  newUrls <- filterM notInSource links
  mapM_ (\url -> push (id, url)) newUrls
  threadid <- liftIO myThreadId
  logMessage $ "Url: " ++ show url ++ "\nWith Thread: " ++ show threadid ++ "\n"


getTarget :: TargetId -> [Target] -> Target
getTarget id targets = head $ filter ((==) id . _targetId) targets


linkIsEligable :: Domain -> [Pattern] -> Url -> Bool
linkIsEligable d ps url = True


extractLinks :: PageData -> [Url]
extractLinks doc = map T.pack links
  where
    links = runLA (hread >>> css "a" ! "href") doc


scrapeDocument :: [Selector] -> PageData -> ResponseCode -> QuedUrl -> Maybe UrlData
scrapeDocument ss doc rc (id, url) = let matches = filter (not . null . documents) $ map f ss
                                         f s = let cssSelector = _selector s
                                                   docs = map T.pack $ extractContent cssSelector doc
                                               in Matches { selector = s, name = (_name s), documents = docs }
                                     in if null matches then Nothing
                                        else Just $ UrlData { url = url, matches = matches, targetId = id, responseCode = rc}


extractContent :: CssSelector -> PageData -> [String]
extractContent s doc = runLA (hread >>> css selector //> getText) doc
  where selector = T.unpack s
