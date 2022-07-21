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

Module      :  Beckn.Types.Core.Taxi.OnConfirm.Order

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Taxi.OnConfirm.Order
  ( module Beckn.Types.Core.Taxi.OnConfirm.Order,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Price as Reexport
import Beckn.Utils.Example
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (State, id, state)

data Order = Order
  { id :: Text,
    items :: [OrderItem],
    estimated_total_fare :: Price
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype OrderItem = OrderItem
  { id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema OrderItem where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example Order where
  example =
    Order
      { id = "ride_booking_id",
        items = [OrderItem "quote_id"],
        estimated_total_fare = example
      }
