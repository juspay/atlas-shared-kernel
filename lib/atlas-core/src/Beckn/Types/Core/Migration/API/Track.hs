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

Module      :  Beckn.Types.Core.Migration.API.Track
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.API.Track where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration.Tracking (Tracking)
import Beckn.Types.Core.ReqTypes (BecknCallbackReq, BecknReq)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))
import Servant.Client (BaseUrl)

type TrackAPI =
  "track"
    :> ReqBody '[JSON] (BecknReq TrackInfo)
    :> Post '[JSON] AckResponse

trackAPI :: Proxy TrackAPI
trackAPI = Proxy

data TrackInfo = TrackInfo
  { order_id :: Text,
    callback_url :: Maybe BaseUrl
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type OnTrackAPI =
  "on_track"
    :> ReqBody '[JSON] (BecknCallbackReq OnTrackInfo)
    :> Post '[JSON] AckResponse

onTrackAPI :: Proxy OnTrackAPI
onTrackAPI = Proxy

newtype OnTrackInfo = OnTrackInfo
  { tracking :: Tracking
  }
  deriving (Generic, Show, FromJSON, ToJSON)
