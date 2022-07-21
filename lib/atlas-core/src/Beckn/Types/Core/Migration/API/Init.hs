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

Module      :  Beckn.Types.Core.Migration.API.Init
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.API.Init where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration.Billing (Billing)
import Beckn.Types.Core.Migration.Fulfillment (Fulfillment)
import Beckn.Types.Core.Migration.ItemQuantity (Quantity)
import Beckn.Types.Core.Migration.Payment (Payment)
import Beckn.Types.Core.Migration.Quotation (Quotation)
import Beckn.Types.Core.ReqTypes (BecknCallbackReq, BecknReq)
import EulerHS.Prelude hiding (id)
import Servant (JSON, Post, ReqBody, (:>))

type InitAPI =
  "init"
    :> ReqBody '[JSON] (BecknReq InitOrderObj)
    :> Post '[JSON] AckResponse

initAPI :: Proxy InitAPI
initAPI = Proxy

newtype InitOrderObj = InitOrderObj {order :: InitOrder}
  deriving (Generic, Show, FromJSON, ToJSON)

data InitOrder = InitOrder
  { items :: [InitOrderItem],
    add_ons :: [IdObject],
    offers :: [IdObject],
    billing :: Billing,
    fulfillment :: Fulfillment
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data InitOrderItem = InitOrderItem
  { id :: Text,
    quantity :: Quantity
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type OnInitAPI =
  "on_init"
    :> ReqBody '[JSON] (BecknCallbackReq InitializedObject)
    :> Post '[JSON] AckResponse

onInitAPI :: Proxy OnInitAPI
onInitAPI = Proxy

newtype InitializedObject = InitializedObject
  { order :: InitializedOrder
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data InitializedOrder = InitializedOrder
  { items :: Maybe [InitOrderItem],
    add_ons :: Maybe [IdObject],
    offers :: Maybe [IdObject],
    billing :: Maybe Billing,
    fulfillment :: Maybe Fulfillment,
    quote :: Maybe Quotation,
    payment :: Maybe Payment
  }
  deriving (Generic, Show, FromJSON, ToJSON)
