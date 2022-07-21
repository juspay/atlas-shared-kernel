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

Module      :  Beckn.Types.Core.Taxi.OnSearch.Catalog
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Taxi.OnSearch.Catalog (Catalog (..)) where

import Beckn.Types.Core.Taxi.OnSearch.Provider (Provider)
import Beckn.Utils.Example
import Beckn.Utils.JSON (slashedRecordFields)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), fromAesonOptions)
import EulerHS.Prelude hiding (exp, id)

newtype Catalog = Catalog
  { bpp_providers :: [Provider]
  }
  deriving (Generic, Show)

instance ToSchema Catalog where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions slashedRecordFields

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields

instance Example Catalog where
  example =
    Catalog
      { bpp_providers = example
      }
