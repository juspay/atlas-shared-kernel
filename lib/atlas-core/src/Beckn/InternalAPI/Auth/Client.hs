{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


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

Module      :  Beckn.InternalAPI.Auth.Client
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.InternalAPI.Auth.Client where

import Beckn.InternalAPI.Auth.API
import Beckn.Prelude
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Error.BaseError.HTTPError.APIError
import Beckn.Utils.Error.Throwing
import qualified EulerHS.Types as E

authAPI :: Text -> E.EulerClient PersonId
authAPI = E.client (Proxy @API)

auth ::
  ( HasField "authServiceUrl" r BaseUrl,
    CoreMetrics m,
    MonadFlow m,
    MonadReader r m
  ) =>
  Text ->
  m PersonId
auth token = do
  url <- asks (.authServiceUrl)
  callOwnAPI Nothing (Just "AUTH_FAILED") url (authAPI token) "auth"
    `catchOwnAPI` throwError . \case
      "INVALID_TOKEN" -> InvalidToken token
      "TOKEN_IS_NOT_VERIFIED" -> TokenIsNotVerified
      "TOKEN_EXPIRED" -> TokenExpired
