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

Module      :  Beckn.Types.Core.Migration.Intent
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.Intent where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Migration.Category (Category)
import Beckn.Types.Core.Migration.Item (Item)
import Beckn.Types.Core.Migration.Location (Location)
import Beckn.Types.Core.Migration.Offer (Offer)
import Beckn.Types.Core.Migration.Payment (Payment)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Types.Core.Migration.Vehicle (Vehicle)
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data Intent = Intent
  { provider :: Maybe ProviderInfo,
    fulfillment :: Maybe FulFillmentInfo,
    payment :: Maybe Payment,
    category :: Maybe Category,
    offer :: Maybe Offer,
    item :: Maybe Item,
    tags :: Maybe Tags
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

emptyIntent :: Intent
emptyIntent =
  Intent
    { provider = Nothing,
      fulfillment = Nothing,
      payment = Nothing,
      category = Nothing,
      offer = Nothing,
      item = Nothing,
      tags = Nothing
    }

instance Example Intent where
  example =
    Intent
      { provider = Nothing,
        fulfillment = example,
        payment = Nothing,
        category = Nothing,
        offer = Nothing,
        item = Nothing,
        tags = Nothing
      }

data ProviderInfo = ProviderInfo
  { id :: Maybe Text,
    descriptor :: Maybe DescriptorName,
    category_id :: Maybe Text,
    locations :: Maybe [IdObject]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype DescriptorName = DescriptorName {name :: Text}
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data FulFillmentInfo = FulFillmentInfo
  { id :: Maybe Text,
    start :: Maybe LocationAndTime,
    end :: Maybe LocationAndTime,
    tags :: Maybe Tags,
    vehicle :: Maybe Vehicle
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

emptyFulFillmentInfo :: FulFillmentInfo
emptyFulFillmentInfo =
  FulFillmentInfo
    { id = Nothing,
      start = Nothing,
      end = Nothing,
      tags = Nothing,
      vehicle = Nothing
    }

instance Example FulFillmentInfo where
  example =
    FulFillmentInfo
      { id = Nothing,
        start = example,
        end = example,
        tags = Nothing,
        vehicle = Nothing
      }

data LocationAndTime = LocationAndTime
  { location :: Maybe Location,
    time :: Maybe Time
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

emptyLocationAndTime :: LocationAndTime
emptyLocationAndTime =
  LocationAndTime
    { location = Nothing,
      time = Nothing
    }

instance Example LocationAndTime where
  example =
    LocationAndTime
      { location = example,
        time = Nothing
      }
