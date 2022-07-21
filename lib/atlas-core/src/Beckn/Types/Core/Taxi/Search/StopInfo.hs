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

Module      :  Beckn.Types.Core.Taxi.Search.StopInfo
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Taxi.Search.StopInfo where

import Beckn.Types.Core.Taxi.Search.Location (Location)
import Beckn.Utils.Example
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)

newtype StopInfo = StopInfo
  { location :: Location
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema StopInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example StopInfo where
  example =
    StopInfo
      { location = example
      }
