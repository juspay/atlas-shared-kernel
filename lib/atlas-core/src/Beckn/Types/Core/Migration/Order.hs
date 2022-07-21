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

Module      :  Beckn.Types.Core.Migration.Order
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.Order where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Migration.Billing
import Beckn.Types.Core.Migration.Fulfillment (Fulfillment)
import Beckn.Types.Core.Migration.ItemQuantity
import Beckn.Types.Core.Migration.Payment
import Beckn.Types.Core.Migration.Quotation
import Beckn.Utils.Example
import Data.Time
import EulerHS.Prelude hiding (State, id, state)

data Order = Order
  { id :: Maybe Text,
    state :: Maybe Text,
    items :: [OrderItem],
    add_ons :: [IdObject],
    offers :: [IdObject],
    billing :: Billing,
    fulfillment :: Fulfillment,
    quote :: Quotation,
    payment :: Payment,
    created_at :: Maybe UTCTime,
    updated_at :: Maybe UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data OrderItem = OrderItem
  { id :: Text,
    quantity :: ItemQuantity
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example Order where
  example =
    Order
      { id = Nothing,
        state = Nothing,
        items = [],
        add_ons = [],
        offers = [],
        billing = example,
        fulfillment = example,
        quote = example,
        payment = example,
        created_at = Nothing,
        updated_at = Nothing
      }

newtype OrderObject = OrderObject
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON)
