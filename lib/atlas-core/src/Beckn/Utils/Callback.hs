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

Module      :  Beckn.Utils.Callback
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Callback (withBecknCallbackMig, WithBecknCallbackMig) where

import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as M.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Error
import Beckn.Types.Error.BaseError.HTTPError.BecknAPIError
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant.Client

someExceptionToCallbackReqMig :: M.Context.Context -> SomeException -> BecknCallbackReq a
someExceptionToCallbackReqMig context exc =
  let BecknAPIError err = someExceptionToBecknApiError exc
   in BecknCallbackReq
        { contents = Left err,
          context
        }

type WithBecknCallbackMig api callback_success m =
  ( MonadFlow m,
    CoreMetrics m,
    HasClient ET.EulerClient api,
    Client ET.EulerClient api
      ~ (BecknCallbackReq callback_success -> ET.EulerClient AckResponse)
  ) =>
  M.Context.Action ->
  Proxy api ->
  M.Context.Context ->
  BaseUrl ->
  m callback_success ->
  m AckResponse

withBecknCallbackMig ::
  (m () -> m ()) ->
  Maybe ET.ManagerSelector ->
  WithBecknCallbackMig api callback_success m
withBecknCallbackMig doWithCallback auth actionName api context cbUrl action = do
  now <- getCurrentTime
  cbAction <-
    M.Context.mapToCbAction actionName
      & fromMaybeM (InternalError $ "Beckn " <> show actionName <> " action doesn't have callback")
  let cbContext =
        context
          & #action .~ cbAction
          & #timestamp .~ now
  forkBecknCallback
    (someExceptionToCallbackReqMig cbContext)
    (BecknCallbackReq cbContext . Right)
    (doWithCallback . void . callBecknAPI auth Nothing (show cbAction) api cbUrl)
    (show actionName)
    action
  return Ack

forkBecknCallback ::
  (Forkable m, MonadCatch m, Log m) =>
  (SomeException -> result) ->
  (success -> result) ->
  (result -> m ()) ->
  Text ->
  m success ->
  m ()
forkBecknCallback fromError fromSuccess doWithResult actionName action =
  fork actionName $
    try action >>= \case
      Right success -> doWithResult $ fromSuccess success
      Left err -> do
        logError $ "Error executing callback action " <> actionName <> ": " <> show err
        doWithResult $ fromError err
