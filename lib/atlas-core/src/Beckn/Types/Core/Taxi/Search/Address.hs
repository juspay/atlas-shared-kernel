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

Module      :  Beckn.Types.Core.Taxi.Search.Address
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Taxi.Search.Address where

import Beckn.Utils.Example
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id, state)

data Address = Address
  { door :: Maybe Text,
    area :: Maybe Text,
    building :: Maybe Text,
    street :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    area_code :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Address where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example Address where
  example =
    Address
      { door = Nothing,
        area = Nothing,
        building = Nothing,
        street = Nothing,
        city = Nothing,
        state = Nothing,
        country = Nothing,
        area_code = Nothing
      }
