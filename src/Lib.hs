module Lib where

import Prelude hiding (putStrLn)
import Data.String.Class (putStrLn)
import Control.Concurrent
import Control.Concurrent.STM
import Network.HTTP.Conduit
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Loops

type Url = String

type ProcessedHashes = TVar (Set.Set String)

type UrlsToProcess = TQueue String

type Links = Int

type WorkerCount = TVar Int

data UrlData = UrlData {hash :: String, url :: Url}
  deriving (Show, Eq)


stringToHash :: String -> UrlData
stringToHash =  error "Not implemented"

writeResults :: UrlData -> FilePath -> IO ()
writeResults _ _ = putStrLn "Do"


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

processUrl:: ProcessedHashes -> Url -> IO Links
processUrl _ _ = do
  threadid <- myThreadId
  putStrLn $ "running: " ++ (show threadid)
  return (0 :: Links)
