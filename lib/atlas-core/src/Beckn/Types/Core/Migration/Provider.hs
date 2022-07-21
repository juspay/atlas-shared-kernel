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

Module      :  Beckn.Types.Core.Migration.Provider
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.Provider (Provider (..)) where

import Beckn.Types.Core.Migration.Category (Category)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Fulfillment (Fulfillment)
import Beckn.Types.Core.Migration.Item (Item)
import Beckn.Types.Core.Migration.Location (Location)
import Beckn.Types.Core.Migration.Offer (Offer)
import Beckn.Types.Core.Migration.Payment (Payment)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Utils.Example
import EulerHS.Prelude hiding (exp, id)

data Provider = Provider
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    category_id :: Maybe Text,
    time :: Maybe Time,
    categories :: Maybe [Category],
    fulfillments :: Maybe [Fulfillment],
    payments :: Maybe [Payment],
    locations :: Maybe [Location],
    offers :: Maybe [Offer],
    items :: Maybe [Item],
    exp :: Maybe Text,
    tags :: Maybe Tags
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example Provider where
  example =
    Provider
      { id = Nothing,
        descriptor = Nothing,
        category_id = Nothing,
        time = Nothing,
        categories = Nothing,
        fulfillments = Nothing,
        payments = Nothing,
        locations = Nothing,
        offers = Nothing,
        items = Nothing,
        exp = Nothing,
        tags = Nothing
      }
