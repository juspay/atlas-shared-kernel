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

Module      :  Beckn.Utils.Servant.Server
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Servant.Server where

import Beckn.Prelude (identity)
import qualified Beckn.Tools.Metrics.Init as Metrics
import Beckn.Types.App (EnvR (..), FlowHandlerR, FlowServerR)
import Beckn.Types.Flow
import Beckn.Types.Time
import Beckn.Utils.App
import qualified Beckn.Utils.FlowLogging as L
import Beckn.Utils.IOLogging
import Beckn.Utils.Logging
import qualified Beckn.Utils.Monitoring.Prometheus.Servant as Metrics
import EulerHS.Prelude
import qualified EulerHS.Runtime as E
import GHC.Records.Extra (HasField)
import Network.Wai.Handler.Warp
  ( Port,
    Settings,
    defaultSettings,
    runSettings,
    setGracefulShutdownTimeout,
    setInstallShutdownHandler,
    setPort,
  )
import Servant
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFailFatal)

class HasEnvEntry r (context :: [Type]) | context -> r where
  getEnvEntry :: Context context -> EnvR r

instance {-# OVERLAPPABLE #-} HasEnvEntry r xs => HasEnvEntry r (notIt ': xs) where
  getEnvEntry (_ :. xs) = getEnvEntry xs

instance {-# OVERLAPPING #-} HasEnvEntry r (EnvR r ': xs) where
  getEnvEntry (x :. _) = x

run ::
  forall a r ctx.
  ( HasContextEntry (ctx .++ '[ErrorFormatters]) ErrorFormatters,
    HasServer a (EnvR r ': ctx)
  ) =>
  Proxy (a :: Type) ->
  FlowServerR r a ->
  Context ctx ->
  EnvR r ->
  Application
run apis server ctx env =
  serveWithContext apis (env :. ctx) $
    hoistServerWithContext apis (Proxy @(EnvR r ': ctx)) f server
  where
    f :: FlowHandlerR r m -> Handler m
    f r = do
      eResult <- liftIO . try $ runReaderT r env
      case eResult of
        Left err ->
          print @String ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res

runGeneric ::
  forall api env m ctx.
  ( HasContextEntry (env ': (ctx .++ '[ErrorFormatters])) ErrorFormatters,
    HasServer api (env ': ctx),
    MonadReader env m
  ) =>
  Proxy (api :: Type) ->
  ServerT api m ->
  Context ctx ->
  env ->
  (forall b. env -> m b -> IO b) ->
  Application
runGeneric apis server ctx env runMonad =
  serveWithContext apis (env :. ctx) $
    hoistServerWithContext apis (Proxy @(env ': ctx)) f server
  where
    f :: m a -> Handler a
    f action = do
      eResult <- liftIO . try $ runMonad env action
      case eResult of
        Left err ->
          print @String ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res

runFlowRDelayedIO :: EnvR r -> FlowR r b -> DelayedIO b
runFlowRDelayedIO env f =
  liftIO (try . runFlowR (flowRuntime env) (appEnv env) $ f)
    >>= either delayedFailFatal pure

runServer ::
  forall env (api :: Type) ctx.
  ( HasField "graceTerminationPeriod" env Seconds,
    HasField "isShuttingDown" env Shutdown,
    HasField "loggerConfig" env L.LoggerConfig,
    HasField "loggerEnv" env LoggerEnv,
    HasField "port" env Port,
    Metrics.SanitizedUrl api,
    HasContextEntry (ctx .++ '[ErrorFormatters]) ErrorFormatters,
    HasServer api (EnvR env ': ctx)
  ) =>
  env ->
  Proxy api ->
  FlowServerR env api ->
  (Application -> Application) ->
  (Settings -> Settings) ->
  Context ctx ->
  (E.FlowRuntime -> IO () -> IO ()) ->
  (env -> IO ()) ->
  (E.FlowRuntime -> FlowR env E.FlowRuntime) ->
  IO ()
runServer appEnv serverAPI serverHandler waiMiddleware waiSettings servantCtx serverStartAction shutdownAction initialize = do
  let port = appEnv.port
  hostname <- getPodName
  let loggerRt = L.getEulerLoggerRuntime hostname $ appEnv.loggerConfig
  let settings =
        defaultSettings
          & setGracefulShutdownTimeout (Just $ getSeconds appEnv.graceTerminationPeriod)
          & setInstallShutdownHandler (handleShutdown appEnv.isShuttingDown (shutdownAction appEnv))
          & setPort port
          & waiSettings
  let server = withModifiedEnv $ \modifiedEnv ->
        run serverAPI serverHandler servantCtx modifiedEnv
          & logRequestAndResponse modifiedEnv
          & Metrics.addServantInfo serverAPI
          & waiMiddleware
  E.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <-
      runFlowR flowRt appEnv $
        initialize flowRt <* logInfo ("Runtime created. Starting server at port " <> show port)
    serverStartAction flowRt' $ runSettings settings $ server (EnvR flowRt' appEnv)

runServerGeneric ::
  forall m env (api :: Type) ctx.
  ( MonadReader env m,
    HasField "graceTerminationPeriod" env Seconds,
    HasField "isShuttingDown" env Shutdown,
    HasField "loggerConfig" env L.LoggerConfig,
    HasField "loggerEnv" env LoggerEnv,
    HasField "port" env Port,
    Metrics.SanitizedUrl api,
    HasContextEntry (env ': (ctx .++ '[ErrorFormatters])) ErrorFormatters,
    HasServer api (env ': ctx)
  ) =>
  env ->
  Proxy api ->
  ServerT api m ->
  (Application -> Application) ->
  (Settings -> Settings) ->
  Context ctx ->
  (env -> IO () -> IO ()) ->
  (env -> IO ()) ->
  (forall q. env -> m q -> IO q) ->
  IO ()
runServerGeneric appEnv serverAPI serverHandler waiMiddleware waiSettings servantCtx serverStartAction shutdownAction runMonad = do
  let port = appEnv.port
  let settings =
        defaultSettings
          & setGracefulShutdownTimeout (Just $ getSeconds appEnv.graceTerminationPeriod)
          & setInstallShutdownHandler (handleShutdown appEnv.isShuttingDown (shutdownAction appEnv))
          & setPort port
          & waiSettings
  let server = withModifiedEnvGeneric $ \modifiedEnv ->
        let loggerFunc = \tag info -> logOutputIO (appendLogTag tag $ modifiedEnv.loggerEnv) INFO info
         in runGeneric serverAPI serverHandler servantCtx modifiedEnv runMonad
              & logRequestAndResponseGeneric loggerFunc
              & Metrics.addServantInfo serverAPI
              & waiMiddleware
  serverStartAction appEnv $ runSettings settings $ server appEnv

type HealthCheckAPI = Get '[JSON] Text

healthCheck :: (Monad m) => ServerT HealthCheckAPI m
healthCheck = pure "App is UP"

runHealthCheckServerWithService ::
  forall env ctx.
  ( HasField "graceTerminationPeriod" env Seconds,
    HasField "isShuttingDown" env Shutdown,
    HasField "loggerConfig" env L.LoggerConfig,
    HasField "loggerEnv" env LoggerEnv,
    HasField "port" env Port,
    HasContextEntry (ctx .++ '[ErrorFormatters]) ErrorFormatters
  ) =>
  env ->
  (Application -> Application) ->
  (Settings -> Settings) ->
  Context ctx ->
  (E.FlowRuntime -> IO ()) ->
  (env -> IO ()) ->
  (E.FlowRuntime -> FlowR env E.FlowRuntime) ->
  IO ()
runHealthCheckServerWithService appEnv waiMiddleware waiSettings servantCtx service =
  runServer appEnv (Proxy @HealthCheckAPI) healthCheck waiMiddleware waiSettings servantCtx forkServerStartService
  where
    forkServerStartService flowRt startServerAction = do
      void $ forkIO startServerAction
      service flowRt

runServerWithHealthCheck ::
  forall env (api :: Type) ctx.
  ( HasField "graceTerminationPeriod" env Seconds,
    HasField "isShuttingDown" env Shutdown,
    HasField "loggerConfig" env L.LoggerConfig,
    HasField "loggerEnv" env LoggerEnv,
    HasField "port" env Port,
    Metrics.SanitizedUrl api,
    HasContextEntry (ctx .++ '[ErrorFormatters]) ErrorFormatters,
    HasServer api (EnvR env ': ctx)
  ) =>
  env ->
  Proxy api ->
  FlowServerR env api ->
  (Application -> Application) ->
  (Settings -> Settings) ->
  Context ctx ->
  (env -> IO ()) ->
  (E.FlowRuntime -> FlowR env E.FlowRuntime) ->
  IO ()
runServerWithHealthCheck appEnv _ serverHandler waiMiddleware waiSettings servantCtx =
  runServer appEnv (Proxy @(HealthCheckAPI :<|> api)) (healthCheck :<|> serverHandler) waiMiddleware waiSettings servantCtx (const identity)
