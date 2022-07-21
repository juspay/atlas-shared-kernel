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

Module      :  Beckn.External.Exotel.Flow
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.External.Exotel.Flow where

import Beckn.External.Exotel.Types
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant hiding (throwError)
import Servant.Client

-- | Exotel API interface
type ExotelConnectAPI =
  BasicAuth "apikey-apitoken" BasicAuthData
    :> ReqBody '[FormUrlEncoded] ExotelRequest
    :> Post '[JSON] ExotelResponse

exotelConnectAPI :: Proxy ExotelConnectAPI
exotelConnectAPI = Proxy

defaultBaseUrl :: ExotelAccountSID -> BaseUrl
defaultBaseUrl sid =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "api.exotel.com",
      baseUrlPort = 443,
      baseUrlPath =
        T.unpack $
          "/v1/Accounts/"
            <> getExotelAccountSID sid
            <> "/Calls/connect.json"
    }

initiateCall ::
  ( CoreMetrics m,
    HasFlowEnv m r '["exotelCfg" ::: Maybe ExotelCfg]
  ) =>
  T.Text ->
  T.Text ->
  BaseUrl ->
  ExotelAttachments ->
  m ExotelResponse
initiateCall from to callbackUrl attachments = do
  withLogTag "Exotel" $ do
    ExotelCfg {..} <- asks (.exotelCfg) >>= fromMaybeM ExotelNotConfigured
    let exoRequest = ExotelRequest from to (getExotelCallerId callerId) callbackUrl attachments
        authData =
          BasicAuthData
            (DT.encodeUtf8 $ getExotelApiKey apiKey)
            (DT.encodeUtf8 $ getExotelApiToken apiToken)
    callExotelAPI
      (defaultBaseUrl sid)
      (callExotel authData exoRequest)
      "initiateCall"
  where
    callExotel authData exoRequest = ET.client exotelConnectAPI authData exoRequest

callExotelAPI :: CallAPI env a
callExotelAPI =
  callApiUnwrappingApiError
    (identity @ExotelError)
    Nothing
    (Just "EXOTEL_NOT_AVAILABLE")
