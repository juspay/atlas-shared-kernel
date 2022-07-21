{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}


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

Module      :  Beckn.Tools.Metrics.Init
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Tools.Metrics.Init where

import Beckn.Utils.Monitoring.Prometheus.Servant
import Data.Text as DT
import EulerHS.Prelude as E
import Network.Wai (Application, Request (..))
import Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.Prometheus
import Prometheus as P
import Prometheus.Metric.GHC (ghcMetrics)
import Prometheus.Metric.Proc

serve :: Int -> IO ()
serve port = do
  _ <- register ghcMetrics
  _ <- register procMetrics
  putStrLn @String $ "Prometheus server started at port " <> show port
  _ <- forkIO $ W.run port metricsApp
  return ()

addServantInfo ::
  SanitizedUrl a =>
  Proxy a ->
  Application ->
  Application
addServantInfo proxy app request respond =
  let mpath = getSanitizedUrl proxy request
      fullpath = DT.intercalate "/" (pathInfo request)
   in instrumentHandlerValue (\_ -> "/" <> fromMaybe fullpath mpath) app request respond
