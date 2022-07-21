{- |
Copyright 2022 Juspay Technologies Pvt Ltd

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Module      :  Beckn.Tools.Metrics.BAPMetrics

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Tools.Metrics.BAPMetrics
  ( module Beckn.Tools.Metrics.BAPMetrics,
    module Reexport,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Tools.Metrics.BAPMetrics.Types as Reexport
import Beckn.Types.Common
import Data.Time (diffUTCTime)
import GHC.Records.Extra
import Prometheus as P

startSearchMetrics :: HasBAPMetrics m r => Text -> m ()
startSearchMetrics txnId = do
  bmContainer <- asks (.bapMetrics)
  startSearchMetrics' bmContainer txnId

finishSearchMetrics :: HasBAPMetrics m r => Text -> m ()
finishSearchMetrics txnId = do
  bmContainer <- asks (.bapMetrics)
  finishSearchMetrics' bmContainer txnId

incrementSearchRequestCount :: HasBAPMetrics m r => m ()
incrementSearchRequestCount = do
  bmContainer <- asks (.bapMetrics)
  incrementCaseCount' bmContainer

incrementCaseCount' :: MonadIO m => BAPMetricsContainer -> m ()
incrementCaseCount' bmContainer = do
  let searchRequestCounter = bmContainer.searchRequestCounter
  liftIO $ P.incCounter searchRequestCounter --is it correct that Euler.runIO = liftIO?

putSearchDuration :: MonadIO m => P.Histogram -> Double -> m ()
putSearchDuration searchDurationHistogram duration = liftIO $ P.observe searchDurationHistogram duration

searchDurationKey :: Text -> Text
searchDurationKey txnId = "atlas:" <> txnId <> ":on_search:received"

searchDurationLockKey :: Text -> Text
searchDurationLockKey txnId = txnId <> ":on_search"

startSearchMetrics' :: MonadFlow m => BAPMetricsContainer -> Text -> m ()
startSearchMetrics' bmContainer txnId = do
  let (_, failureCounter) = bmContainer.searchDuration
      searchRedisExTime = getSeconds bmContainer.searchDurationTimeout
  startTime <- getCurrentTime
  Redis.setExRedis (searchDurationKey txnId) startTime (searchRedisExTime + 1) -- a bit more time to
  -- allow forked thread to handle failure
  fork "Gateway Search Metrics" $ do
    liftIO $ threadDelay $ searchRedisExTime * 1000000
    whenM (Redis.tryLockRedis (searchDurationLockKey txnId) searchRedisExTime) $ do
      Redis.getKeyRedis (searchDurationKey txnId) >>= \case
        Just (_ :: UTCTime) -> do
          void $ Redis.deleteKeyRedis (searchDurationKey txnId)
          liftIO $ P.incCounter failureCounter
        Nothing -> return ()
      Redis.unlockRedis $ searchDurationLockKey txnId

finishSearchMetrics' :: MonadFlow m => BAPMetricsContainer -> Text -> m ()
finishSearchMetrics' bmContainer txnId = do
  let (searchDurationHistogram, _) = bmContainer.searchDuration
      searchRedisExTime = getSeconds bmContainer.searchDurationTimeout
  endTime <- getCurrentTime
  whenM (Redis.tryLockRedis (searchDurationLockKey txnId) searchRedisExTime) $ do
    Redis.getKeyRedis (searchDurationKey txnId) >>= \case
      Just startTime -> do
        void $ Redis.deleteKeyRedis (searchDurationKey txnId)
        putSearchDuration searchDurationHistogram . realToFrac . diffUTCTime endTime $ startTime
      Nothing -> return ()
    Redis.unlockRedis $ searchDurationLockKey txnId
