{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}


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

Module      :  Beckn.Utils.Error.BaseError.HTTPError.APIError
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Error.BaseError.HTTPError.APIError where

import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Utils.Error.Throwing
import Beckn.Utils.Logging
import Beckn.Utils.Servant.Client
import EulerHS.Prelude
import qualified EulerHS.Types as ET

newtype APICallError = APICallError APIError
  deriving (Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''APICallError

instance IsBaseError APICallError where
  toMessage (APICallError APIError {..}) =
    Just $
      "Request to own API returned error code " <> errorCode
        <> maybe "" (" with message: " <>) errorMessage

instance IsHTTPError APICallError where
  toErrorCode (APICallError _) = "API_CALL_ERROR"

instance IsAPIError APICallError

callOwnAPI ::
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  CallAPI env a
callOwnAPI = callApiUnwrappingApiError APICallError

catchOwnAPI ::
  ( HasCallStack,
    MonadCatch m,
    Log m
  ) =>
  m a ->
  (Text -> m a) ->
  m a
catchOwnAPI m f = m `safeCatch` \(APICallError APIError {errorCode}) -> f errorCode

infixl 1 `catchOwnAPI`

toAPIError :: (IsHTTPError e, IsAPIError e) => e -> APIError
toAPIError e =
  APIError
    { errorCode = toErrorCode e,
      errorMessage = toMessageIfNotInternal e,
      errorPayload = toPayload e
    }
