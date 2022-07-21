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

Module      :  Beckn.Types.Core.Taxi.Search.Location

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Taxi.Search.Location
  ( module Beckn.Types.Core.Taxi.Search.Location,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Gps as Reexport
import Beckn.Types.Core.Taxi.Search.Address (Address)
import Beckn.Utils.Example
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude

data Location = Location
  { gps :: Gps,
    address :: Address
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Location where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example Location where
  example =
    Location
      { gps = example,
        address = example
      }
