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

Module      :  Beckn.Tools.Metrics.CoreMetrics.Types

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Tools.Metrics.CoreMetrics.Types
  ( HasCoreMetrics,
    CoreMetrics (..),
    CoreMetricsContainer (..),
    registerCoreMetricsContainer,
  )
where

import Beckn.Types.Time (Milliseconds)
import EulerHS.Prelude as E
import GHC.Records.Extra
import Prometheus as P
import Servant.Client (BaseUrl, ClientError)

type RequestLatencyMetric = P.Vector P.Label3 P.Histogram

type ErrorCounterMetric = P.Vector P.Label3 P.Counter

type URLCallRetriesMetric = P.Vector P.Label2 P.Counter

type URLCallRetryFailuresMetric = P.Vector P.Label1 P.Counter

type HasCoreMetrics r =
  ( HasField "coreMetrics" r CoreMetricsContainer
  )

class CoreMetrics m where
  addRequestLatency ::
    Text ->
    Text ->
    Milliseconds ->
    Either ClientError a ->
    m ()
  incrementErrorCounter :: Text -> SomeException -> m ()
  addUrlCallRetries :: BaseUrl -> Int -> m ()
  addUrlCallRetryFailures :: BaseUrl -> m ()

data CoreMetricsContainer = CoreMetricsContainer
  { requestLatency :: RequestLatencyMetric,
    errorCounter :: ErrorCounterMetric,
    urlCallRetries :: URLCallRetriesMetric,
    urlCallRetryFailures :: URLCallRetryFailuresMetric
  }

registerCoreMetricsContainer :: IO CoreMetricsContainer
registerCoreMetricsContainer = do
  requestLatency <- registerRequestLatencyMetric
  errorCounter <- registerErrorCounterMetric
  urlCallRetries <- registerURLCallRetriesMetric
  urlCallRetryFailures <- registerURLCallRetryFailuresMetric

  return CoreMetricsContainer {..}

registerRequestLatencyMetric :: IO RequestLatencyMetric
registerRequestLatencyMetric =
  P.register $
    P.vector ("host", "service", "status") $
      P.histogram info P.defaultBuckets
  where
    info = P.Info "external_request_duration" ""

registerErrorCounterMetric :: IO ErrorCounterMetric
registerErrorCounterMetric =
  P.register $
    P.vector ("HttpCode", "ErrorContext", "ErrorCode") $
      P.counter info
  where
    info = P.Info "error_counter" ""

registerURLCallRetriesMetric :: IO URLCallRetriesMetric
registerURLCallRetriesMetric =
  P.register $
    P.vector ("URL", "RetryCount") $
      P.counter info
  where
    info = P.Info "url_call_retries_counter" ""

registerURLCallRetryFailuresMetric :: IO URLCallRetryFailuresMetric
registerURLCallRetryFailuresMetric =
  P.register $
    P.vector "URL" $
      P.counter info
  where
    info = P.Info "url_call_retry_failures_counter" ""
