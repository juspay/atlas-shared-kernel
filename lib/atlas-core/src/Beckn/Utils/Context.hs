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

Module      :  Beckn.Utils.Context
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Context where

import Beckn.Types.App
import qualified Beckn.Types.Core.Context as Cab
import Beckn.Types.MonadGuid
import Beckn.Types.Time
import EulerHS.Prelude

buildTaxiContext ::
  (MonadTime m, MonadGuid m) =>
  Cab.Action ->
  Text ->
  Maybe Text ->
  Text ->
  BaseUrl ->
  Maybe Text ->
  Maybe BaseUrl ->
  m Cab.Context
buildTaxiContext action msgId txnId bapId bapUri bppId bppUri = do
  currTime <- getCurrentTime
  return $
    Cab.Context
      { domain = Cab.MOBILITY,
        action,
        core_version = "0.9.3",
        bap_id = bapId,
        bap_uri = bapUri,
        bpp_id = bppId,
        bpp_uri = bppUri,
        transaction_id = txnId,
        message_id = msgId,
        timestamp = currTime,
        country = "IND",
        city = "Kochi"
      }
