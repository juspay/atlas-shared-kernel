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

Module      :  Beckn.Utils.Servant.Client
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Servant.Client where

import qualified Beckn.Tools.Metrics.CoreMetrics as Metrics
import Beckn.Types.Common
import Beckn.Types.Error (ExternalAPICallError (..))
import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Error.BaseError.HTTPError.CallAPIError
import Beckn.Types.Error.BaseError.HTTPError.FromResponse
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Error.Throwing
import Beckn.Utils.Logging
import Beckn.Utils.Servant.BaseUrl
import Beckn.Utils.Time
import qualified Data.Aeson as A
import qualified Data.Map.Strict as Map
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as ET
import GHC.Records.Extra (HasField)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import Servant.Client.Core

data HttpClientOptions = HttpClientOptions
  { timeoutMs :: Int,
    maxRetries :: Int
  }
  deriving (Generic, FromDhall)

type HasHttpClientOptions r c = HasField "httpClientOptions" r HttpClientOptions

type CallAPI' m res res' =
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    ET.JSONEx res,
    ToJSON res
  ) =>
  BaseUrl ->
  ET.EulerClient res ->
  Text ->
  m res'

type CallAPI m res = CallAPI' m res res

callAPI ::
  CallAPI' m res (Either ClientError res)
callAPI = callAPI' Nothing

callAPI' ::
  Maybe ET.ManagerSelector ->
  CallAPI' m res (Either ClientError res)
callAPI' mbManagerSelector baseUrl eulerClient desc =
  withLogTag "callAPI" $ do
    let managerSelector = fromMaybe defaultHttpManager mbManagerSelector
    res <-
      measuringDuration (Metrics.addRequestLatency (showBaseUrlText baseUrl) desc) $
        L.callAPI' (Just managerSelector) baseUrl eulerClient
    case res of
      Right r -> logDebug $ "Ok response: " <> decodeUtf8 (A.encode r)
      Left err -> logDebug $ "Error occured during client call: " <> show err
    return res

callApiExtractingApiError ::
  ( Metrics.CoreMetrics m,
    FromResponse err
  ) =>
  Maybe ET.ManagerSelector ->
  CallAPI' m a (Either (CallAPIError err) a)
callApiExtractingApiError mbManagerSelector baseUrl eulerClient desc =
  callAPI' mbManagerSelector baseUrl eulerClient desc
    <&> extractApiError

callApiUnwrappingApiError ::
  ( Metrics.CoreMetrics m,
    FromResponse err,
    IsHTTPException exc
  ) =>
  (err -> exc) ->
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  CallAPI m a
callApiUnwrappingApiError toAPIException mbManagerSelector errorCodeMb baseUrl eulerClient desc =
  callApiExtractingApiError mbManagerSelector baseUrl eulerClient desc
    >>= unwrapEitherCallAPIError errorCodeMb baseUrl toAPIException

defaultHttpManager :: String
defaultHttpManager = "default"

setResponseTimeout :: Int -> Http.ManagerSettings -> Http.ManagerSettings
setResponseTimeout timeout settings =
  settings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)}

createManagers ::
  ( MonadReader r m,
    HasHttpClientOptions r c,
    MonadFlow m
  ) =>
  Map String Http.ManagerSettings ->
  m (Map String Http.Manager)
createManagers managerSettings = do
  timeout <- asks (.httpClientOptions.timeoutMs)
  liftIO $ managersFromManagersSettings timeout managerSettings

managersFromManagersSettings ::
  Int ->
  Map String Http.ManagerSettings ->
  IO (Map String Http.Manager)
managersFromManagersSettings timeout =
  mapM Http.newManager
    . fmap (setResponseTimeout timeout)
    . Map.insert defaultHttpManager Http.tlsManagerSettings

catchConnectionErrors :: (MonadCatch m, Log m) => m a -> (ExternalAPICallError -> m a) -> m a
catchConnectionErrors action errorHandler =
  action `catch` \err -> do
    case err.clientError of
      ConnectionError _ -> errorHandler err
      _ -> throwError err

retryAction ::
  ( MonadCatch m,
    Metrics.CoreMetrics m,
    Log m
  ) =>
  ExternalAPICallError ->
  Int ->
  Int ->
  m a ->
  m a
retryAction currentErr currentRetryCount maxRetries action = do
  logWarning $ "Error calling " <> showBaseUrlText currentErr.baseUrl <> ": " <> show currentErr.clientError
  logWarning $ "Retrying attempt " <> show currentRetryCount <> " calling " <> showBaseUrlText currentErr.baseUrl
  Metrics.addUrlCallRetries currentErr.baseUrl currentRetryCount
  catchConnectionErrors action $ \err -> do
    if currentRetryCount < maxRetries
      then retryAction err (currentRetryCount + 1) maxRetries action
      else do
        logError $ "Maximum of retrying attempts is reached calling " <> showBaseUrlText err.baseUrl
        Metrics.addUrlCallRetryFailures currentErr.baseUrl
        throwError err

withRetry ::
  ( MonadCatch m,
    MonadReader r m,
    HasHttpClientOptions r c,
    Metrics.CoreMetrics m,
    Log m
  ) =>
  m a ->
  m a
withRetry action = do
  maxRetries <- asks (.httpClientOptions.maxRetries)
  catchConnectionErrors action $ \err -> do
    if maxRetries > 0
      then retryAction err 1 maxRetries action
      else do
        Metrics.addUrlCallRetryFailures err.baseUrl
        throwError err
