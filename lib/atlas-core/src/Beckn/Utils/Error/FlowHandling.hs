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

Module      :  Beckn.Utils.Error.FlowHandling

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Error.FlowHandling
  ( withFlowHandler,
    withFlowHandlerAPI,
    withFlowHandlerBecknAPI,
    apiHandler,
    becknApiHandler,
    someExceptionToBecknApiError,
    handleIfUp,
    throwServantError,
  )
where

import Beckn.Tools.Metrics.CoreMetrics (HasCoreMetrics)
import qualified Beckn.Tools.Metrics.CoreMetrics as Metrics
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Error as Err
import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Flow
import Beckn.Utils.Error.BaseError.HTTPError.APIError (toAPIError)
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError (toBecknAPIError)
import Beckn.Utils.Logging
import Control.Concurrent.STM (isEmptyTMVar)
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records.Extra
import Network.HTTP.Types (Header, hContentType)
import Network.HTTP.Types.Header (HeaderName)
import Servant (ServerError (..))

withFlowHandler :: FlowR r a -> FlowHandlerR r a
withFlowHandler flow = do
  (EnvR flowRt appEnv) <- ask
  liftIO . runFlowR flowRt appEnv $ flow

withFlowHandlerAPI ::
  ( Metrics.CoreMetrics (FlowR r),
    HasField "isShuttingDown" r (TMVar ()),
    Log (FlowR r)
  ) =>
  FlowR r a ->
  FlowHandlerR r a
withFlowHandlerAPI = withFlowHandler . apiHandler . handleIfUp

withFlowHandlerBecknAPI ::
  ( HasCoreMetrics r,
    HasField "isShuttingDown" r (TMVar ()),
    Log (FlowR r)
  ) =>
  FlowR r AckResponse ->
  FlowHandlerR r AckResponse
withFlowHandlerBecknAPI = withFlowHandler . becknApiHandler . handleIfUp

handleIfUp ::
  ( L.MonadFlow m,
    Log m,
    MonadReader r m,
    HasField "isShuttingDown" r (TMVar ()),
    Metrics.CoreMetrics m
  ) =>
  m a ->
  m a
handleIfUp flow = do
  shutdown <- asks (.isShuttingDown)
  shouldRun <- L.runIO $ atomically $ isEmptyTMVar shutdown
  if shouldRun
    then flow
    else throwAPIError ServerUnavailable

apiHandler ::
  ( MonadCatch m,
    Log m,
    Metrics.CoreMetrics m
  ) =>
  m a ->
  m a
apiHandler = (`catch` someExceptionToAPIErrorThrow)

becknApiHandler ::
  ( MonadCatch m,
    Log m,
    Metrics.CoreMetrics m
  ) =>
  m a ->
  m a
becknApiHandler = (`catch` someExceptionToBecknApiErrorThrow)

someExceptionToAPIErrorThrow ::
  ( MonadCatch m,
    Log m,
    Metrics.CoreMetrics m
  ) =>
  SomeException ->
  m a
someExceptionToAPIErrorThrow exc
  | Just (HTTPException err) <- fromException exc = throwAPIError err
  | Just (BaseException err) <- fromException exc =
    throwAPIError . InternalError . fromMaybe (show err) $ toMessage err
  | otherwise = throwAPIError . InternalError $ show exc

someExceptionToBecknApiErrorThrow ::
  ( MonadCatch m,
    Log m,
    Metrics.CoreMetrics m
  ) =>
  SomeException ->
  m a
someExceptionToBecknApiErrorThrow exc
  | Just (HTTPException err) <- fromException exc = throwBecknApiError err
  | otherwise =
    throwBecknApiError . InternalError $ show exc

someExceptionToBecknApiError :: SomeException -> BecknAPIError
someExceptionToBecknApiError exc
  | Just (HTTPException err) <- fromException exc = toBecknAPIError err
  | otherwise = toBecknAPIError . InternalError $ show exc

throwAPIError ::
  ( Log m,
    MonadThrow m,
    IsHTTPException e,
    Exception e,
    Metrics.CoreMetrics m
  ) =>
  e ->
  m a
throwAPIError = throwHTTPError toAPIError

throwBecknApiError ::
  ( Log m,
    MonadThrow m,
    IsHTTPException e,
    Exception e,
    Metrics.CoreMetrics m
  ) =>
  e ->
  m a
throwBecknApiError = throwHTTPError toBecknAPIError

throwHTTPError ::
  ( ToJSON j,
    Log m,
    MonadThrow m,
    IsHTTPException e,
    Exception e,
    Metrics.CoreMetrics m
  ) =>
  (e -> j) ->
  e ->
  m b
throwHTTPError toJsonError err = do
  let someExc = toException err
  logError $ makeLogSomeException someExc
  Metrics.incrementErrorCounter "DEFAULT_ERROR" someExc
  throwServantError (toHttpCode err) (toCustomHeaders err) (toJsonError err)

throwServantError ::
  (ToJSON a, Log m, MonadThrow m) =>
  HttpCode ->
  [Header] ->
  a ->
  m b
throwServantError httpCode customHeaders jsonError = withLogTag "HTTP_ERROR" $ do
  let body = A.encode jsonError
  let serverErr = toServerError httpCode
  throwM
    serverErr
      { errBody = body,
        errHeaders = jsonHeader : customHeaders ++ errHeaders serverErr
      }
  where
    jsonHeader :: (HeaderName, ByteString)
    jsonHeader = (hContentType, "application/json;charset=utf-8")
