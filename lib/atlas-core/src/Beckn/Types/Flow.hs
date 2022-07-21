{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}


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

Module      :  Beckn.Types.Flow
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Flow (FlowR, runFlowR) where

import qualified Beckn.Tools.Metrics.CoreMetrics as Metrics
import Beckn.Types.Forkable
import Beckn.Types.Logging
import Beckn.Types.MonadGuid
import Beckn.Types.Time
import qualified Beckn.Utils.IOLogging as IOLogging
import Beckn.Utils.Logging
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Prometheus (MonadMonitor (..))

runFlowR :: R.FlowRuntime -> r -> FlowR r a -> IO a
runFlowR flowRt r (FlowR x) = I.runFlow flowRt . runReaderT x $ r

newtype FlowR r a = FlowR {unFlowR :: ReaderT r L.Flow a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader r,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

instance L.MonadFlow (FlowR r) where
  {-# INLINEABLE callServantAPI #-}
  callServantAPI mbMgrSel url cl = FlowR $ L.callServantAPI mbMgrSel url cl
  {-# INLINEABLE callHTTPWithCert #-}
  callHTTPWithCert url cert = FlowR $ L.callHTTPWithCert url cert
  {-# INLINEABLE evalLogger' #-}
  evalLogger' logAct = FlowR $ L.evalLogger' logAct
  {-# INLINEABLE runIO' #-}
  runIO' descr ioAct = FlowR $ L.runIO' descr ioAct
  {-# INLINEABLE getOption #-}
  getOption k = FlowR $ L.getOption k
  {-# INLINEABLE setOption #-}
  setOption k v = FlowR $ L.setOption k v
  {-# INLINEABLE delOption #-}
  delOption k = FlowR $ L.delOption k
  {-# INLINEABLE generateGUID #-}
  generateGUID = FlowR L.generateGUID
  {-# INLINEABLE runSysCmd #-}
  runSysCmd cmd = FlowR $ L.runSysCmd cmd
  {-# INLINEABLE initSqlDBConnection #-}
  initSqlDBConnection cfg = FlowR $ L.initSqlDBConnection cfg
  {-# INLINEABLE deinitSqlDBConnection #-}
  deinitSqlDBConnection conn = FlowR $ L.deinitSqlDBConnection conn
  {-# INLINEABLE getSqlDBConnection #-}
  getSqlDBConnection cfg = FlowR $ L.getSqlDBConnection cfg
  {-# INLINEABLE initKVDBConnection #-}
  initKVDBConnection cfg = FlowR $ L.initKVDBConnection cfg
  {-# INLINEABLE deinitKVDBConnection #-}
  deinitKVDBConnection conn = FlowR $ L.deinitKVDBConnection conn
  {-# INLINEABLE getKVDBConnection #-}
  getKVDBConnection cfg = FlowR $ L.getKVDBConnection cfg
  {-# INLINEABLE runDB #-}
  runDB conn dbAct = FlowR $ L.runDB conn dbAct
  {-# INLINEABLE runTransaction #-}
  runTransaction conn dbAct = FlowR $ L.runTransaction conn dbAct
  {-# INLINEABLE await #-}
  await mbMcs awaitable = FlowR $ L.await mbMcs awaitable
  {-# INLINEABLE runSafeFlow #-}
  runSafeFlow flow = FlowR $ L.runSafeFlow flow
  {-# INLINEABLE runKVDB #-}
  runKVDB cName act = FlowR $ L.runKVDB cName act
  {-# INLINEABLE runPubSub #-}
  runPubSub act = FlowR $ L.runPubSub act
  {-# INLINEABLE publish #-}
  publish channel payload = FlowR $ L.publish channel payload
  {-# INLINEABLE subscribe #-}
  subscribe channels cb = FlowR $ L.subscribe channels cb
  {-# INLINEABLE psubscribe #-}
  psubscribe channels cb = FlowR $ L.psubscribe channels cb
  {-# INLINEABLE withModifiedRuntime #-}
  withModifiedRuntime f flow = FlowR $ L.withModifiedRuntime f flow

instance MonadIO (FlowR r) where
  liftIO = FlowR . L.runIO

instance {-# OVERLAPPABLE #-} IOLogging.HasLog r => Log (FlowR r) where
  logOutput = IOLogging.logOutputImplementation
  withLogTag lc (FlowR flowR) = FlowR $ IOLogging.withLogTagImplementation lc flowR

instance MonadTime (FlowR r) where
  getCurrentTime = liftIO getCurrentTime

instance MonadClock (FlowR r) where
  getClockTime = liftIO getClockTime

instance Metrics.HasCoreMetrics r => Metrics.CoreMetrics (FlowR r) where
  addRequestLatency = Metrics.addRequestLatencyImplementation
  incrementErrorCounter = Metrics.incrementErrorCounterImplementation
  addUrlCallRetries = Metrics.addUrlCallRetriesImplementation
  addUrlCallRetryFailures = Metrics.addUrlCallFailuresImplementation

instance MonadMonitor (FlowR r) where
  doIO = liftIO

instance MonadGuid (FlowR r) where
  generateGUIDText = FlowR L.generateGUID

instance (Log (FlowR r), Metrics.CoreMetrics (FlowR r)) => Forkable (FlowR r) where
  fork tag f = do
    FlowR $ ReaderT $ L.forkFlow tag . runReaderT (unFlowR $ handleExc f)
    where
      handleExc = try >=> (`whenLeft` err)
      err (e :: SomeException) = do
        logError $ "Thread " <> show tag <> " died with error: " <> makeLogSomeException e
        Metrics.incrementErrorCounter "FORKED_THREAD_ERROR" e
