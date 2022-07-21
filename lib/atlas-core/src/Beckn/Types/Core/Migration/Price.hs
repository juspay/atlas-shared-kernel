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

Module      :  Beckn.Types.Core.Migration.Price
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.Price (Price (..)) where

import Beckn.Types.Core.Migration.DecimalValue (DecimalValue)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data Price = Price
  { currency :: Maybe Text,
    value :: Maybe DecimalValue,
    estimated_value :: Maybe DecimalValue,
    computed_value :: Maybe DecimalValue,
    listed_value :: Maybe DecimalValue,
    offered_value :: Maybe DecimalValue,
    minimum_value :: Maybe DecimalValue,
    maximum_value :: Maybe DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
