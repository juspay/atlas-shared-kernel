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

Module      :  Beckn.Types.Core.Taxi.OnSearch.Provider

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Taxi.OnSearch.Provider
  ( Provider (..),
  )
where

import Beckn.Types.Core.Taxi.OnSearch.Item (Item)
import Beckn.Utils.Example
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (exp, id)

data Provider = Provider
  { id :: Text,
    name :: Text,
    items :: [Item], --FIXME this should be list of only RENTAL or only ONE_WAY items
    contacts :: Text,
    category_id :: Text, -- ONE_WAY or RENTAL
    rides_inprogress :: Int,
    rides_completed :: Int,
    rides_confirmed :: Int
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Provider where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example Provider where
  example =
    Provider
      { id = "id",
        name = "name",
        items = [],
        contacts = "99999999999",
        category_id = "ONE_WAY",
        rides_inprogress = 12,
        rides_completed = 32,
        rides_confirmed = 3
      }
