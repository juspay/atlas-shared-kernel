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

Module      :  Beckn.Types.Core.Migration.Catalog
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.Catalog (Catalog (..)) where

import Beckn.Types.Core.Migration.Category (Category)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Fulfillment (Fulfillment)
import Beckn.Types.Core.Migration.Offer (Offer)
import Beckn.Types.Core.Migration.Payment (Payment)
import Beckn.Types.Core.Migration.Provider (Provider)
import Beckn.Utils.Example
import Beckn.Utils.JSON (slashedRecordFields)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (exp, id)

data Catalog = Catalog
  { bpp_descriptor :: Maybe Descriptor,
    bpp_categories :: Maybe [Category],
    bpp_fulfillments :: Maybe [Fulfillment],
    bpp_payments :: Maybe [Payment],
    bpp_offers :: Maybe [Offer],
    bpp_providers :: Maybe [Provider],
    exp :: Maybe UTCTime
  }
  deriving (Generic, Show)

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields

instance Example Catalog where
  example =
    Catalog
      { bpp_descriptor = Nothing,
        bpp_categories = Nothing,
        bpp_fulfillments = Nothing,
        bpp_payments = Nothing,
        bpp_offers = Nothing,
        bpp_providers = Just $ one example,
        exp = Nothing
      }
