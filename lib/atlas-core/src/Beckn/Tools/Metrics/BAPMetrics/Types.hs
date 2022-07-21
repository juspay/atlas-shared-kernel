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

Module      :  Beckn.Tools.Metrics.BAPMetrics.Types

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Tools.Metrics.BAPMetrics.Types
  ( HasBAPMetrics,
    BAPMetricsContainer (..),
    registerBAPMetricsContainer,
  )
where

import Beckn.Prelude
import Beckn.Utils.Common
import Prometheus as P

type HasBAPMetrics m r = (HasFlowEnv m r '["bapMetrics" ::: BAPMetricsContainer])

data BAPMetricsContainer = BAPMetricsContainer
  { searchRequestCounter :: SearchRequestCounterMetric,
    searchDurationTimeout :: Seconds,
    searchDuration :: SearchDurationMetric
  }

type SearchRequestCounterMetric = P.Counter

type SearchDurationMetric = (P.Histogram, P.Counter)

registerBAPMetricsContainer :: Seconds -> IO BAPMetricsContainer
registerBAPMetricsContainer searchDurationTimeout = do
  searchRequestCounter <- registerSearchRequestCounterMetric
  searchDuration <- registerSearchDurationMetric searchDurationTimeout
  return $ BAPMetricsContainer {..}

registerSearchRequestCounterMetric :: IO SearchRequestCounterMetric
registerSearchRequestCounterMetric = P.register $ P.counter $ P.Info "search_request_count" ""

registerSearchDurationMetric :: Seconds -> IO SearchDurationMetric
registerSearchDurationMetric searchDurationTimeout = do
  let bucketsCount = (getSeconds searchDurationTimeout + 1) * 2
  searchDurationHistogram <- P.register . P.histogram (P.Info "beckn_search_round_trip" "") $ P.linearBuckets 0 0.5 bucketsCount
  failureCounter <- P.register $ P.counter $ P.Info "beckn_search_round_trip_failure_counter" ""
  return (searchDurationHistogram, failureCounter)
