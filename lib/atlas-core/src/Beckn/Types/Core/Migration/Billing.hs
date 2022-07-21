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

Module      :  Beckn.Types.Core.Migration.Billing

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.Billing
  ( Billing (..),
  )
where

import Beckn.Types.Core.Migration.Address (Address)
import Beckn.Types.Core.Migration.Organization (Organization)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Utils.Example
import Data.Time (UTCTime)
import EulerHS.Prelude

data Billing = Billing
  { name :: Text,
    organization :: Maybe Organization,
    address :: Maybe Address,
    email :: Maybe Text,
    phone :: Text,
    time :: Maybe Time,
    tax_number :: Maybe Text,
    created_at :: Maybe UTCTime,
    updated_at :: Maybe UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example Billing where
  example =
    Billing
      { name = "Mr. Payeerson",
        organization = Nothing,
        address = Nothing,
        email = Nothing,
        phone = "+919999999999",
        time = Nothing,
        tax_number = Nothing,
        created_at = Nothing,
        updated_at = Nothing
      }
