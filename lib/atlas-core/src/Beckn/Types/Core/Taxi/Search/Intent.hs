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

Module      :  Beckn.Types.Core.Taxi.Search.Intent

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Taxi.Search.Intent
  ( module Beckn.Types.Core.Taxi.Search.Intent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Search.StartInfo
import Beckn.Types.Core.Taxi.Search.StopInfo
import Beckn.Utils.Example
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)

newtype Intent = Intent
  { fulfillment :: FulFillmentInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Intent where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example Intent where
  example =
    Intent
      { fulfillment = example
      }

-- If end = Nothing, then bpp sends quotes only for RENTAL
-- If end is Just, then bpp sends quotes both for RENTAL and ONE_WAY
data FulFillmentInfo = FulFillmentInfo
  { start :: StartInfo,
    end :: Maybe StopInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema FulFillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example FulFillmentInfo where
  example =
    FulFillmentInfo
      { start = example,
        end = example
      }
