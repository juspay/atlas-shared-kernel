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

Module      :  Beckn.Scheduler.Metrics
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Scheduler.Metrics where

import Beckn.Prelude
import qualified Prometheus as P

newtype SchedulerMetrics = SchedulerMetrics
  { durationHistogram :: P.Histogram
  }

setupSchedulerMetrics :: MonadIO m => m SchedulerMetrics
setupSchedulerMetrics = do
  let histInfo = P.Info "job_execution_duration" "Duration of the job execution"
  durationHistogram <- P.register $ P.histogram histInfo P.defaultBuckets
  pure $ SchedulerMetrics {..}

observeJobExecDuration :: (MonadIO m, MonadReader r m, HasField "metrics" r SchedulerMetrics) => Double -> m ()
observeJobExecDuration duration = do
  metrics <- asks (.metrics)
  liftIO $ P.observe metrics.durationHistogram duration
