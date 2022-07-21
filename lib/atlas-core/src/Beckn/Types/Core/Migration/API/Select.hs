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

Module      :  Beckn.Types.Core.Migration.API.Select
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.API.Select where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration.AddOn (AddOn)
import Beckn.Types.Core.Migration.Item (Item)
import Beckn.Types.Core.Migration.ItemQuantity (ItemQuantity)
import Beckn.Types.Core.Migration.Offer (Offer)
import Beckn.Types.Core.Migration.Quotation (Quotation)
import Beckn.Types.Core.ReqTypes (BecknCallbackReq, BecknReq)
import Beckn.Utils.JSON (uniteObjects)
import Data.Aeson (withObject, (.:))
import Data.Aeson.Types (parseFail)
import EulerHS.Prelude hiding (id)
import Servant (JSON, Post, ReqBody, (:>))

type SelectAPI =
  "select"
    :> ReqBody '[JSON] (BecknReq SelectedObject)
    :> Post '[JSON] AckResponse

selectAPI :: Proxy SelectAPI
selectAPI = Proxy

newtype SelectedObject = SelectedObject
  { order :: SelectedOrder
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data SelectedOrder = SelectedOrder
  { items :: [SelectedItem],
    add_ons :: Maybe [IdObject],
    offers :: Maybe [IdObject]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data SelectedItem = SelectedItem
  { id :: Text,
    quantity :: Int
  }
  deriving (Generic, Show, ToJSON)

instance FromJSON SelectedItem where
  parseJSON = withObject "RatingInfo" $ \obj -> do
    quantity <- obj .: "quantity"
    unless (quantity >= 0) $ parseFail "Expected quantity to be >= 0."
    itemId <- obj .: "id"
    pure $ SelectedItem itemId quantity

type OnSelectAPI =
  "on_select"
    :> ReqBody '[JSON] (BecknCallbackReq SelectedCallbackObject)
    :> Post '[JSON] AckResponse

onSelectAPI :: Proxy OnSelectAPI
onSelectAPI = Proxy

newtype SelectedCallbackObject = SelectedCallbackObject
  { order :: SelectedOrderCallback
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data SelectedOrderCallback = SelectedOrderCallback
  { items :: Maybe [SelectedCallbackItem],
    add_ons :: Maybe [AddOn],
    offers :: Maybe [Offer],
    quote :: Maybe Quotation
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data SelectedCallbackItem = SelectedCallbackItem Item ItemQuantity
  deriving (Generic, Show)

instance FromJSON SelectedCallbackItem where
  parseJSON obj = SelectedCallbackItem <$> parseJSON obj <*> parseJSON obj

instance ToJSON SelectedCallbackItem where
  toJSON (SelectedCallbackItem item itemQuantity) = uniteObjects [toJSON item, toJSON itemQuantity]
