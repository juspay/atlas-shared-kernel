{-# LANGUAGE TemplateHaskell #-}


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

Module      :  Beckn.Types.Core.Taxi.Common.CancellationSource
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Taxi.Common.CancellationSource where

import Beckn.Storage.Esqueleto
import Data.Aeson
import Data.OpenApi
import EulerHS.Prelude

data CancellationSource
  = ByUser
  | ByDriver
  | ByOrganization
  | ByAllocator
  deriving (Show, Eq, Ord, Read, Generic)

derivePersistField "CancellationSource"

instance ToJSON CancellationSource where
  toJSON = genericToJSON cancellationSourceJSONOptions

instance FromJSON CancellationSource where
  parseJSON = genericParseJSON cancellationSourceJSONOptions

cancellationSourceJSONOptions :: Options
cancellationSourceJSONOptions =
  defaultOptions
    { constructorTagModifier = \case
        "ByUser" -> "CANCELLED_BY_USER"
        "ByDriver" -> "CANCELLED_BY_DRIVER"
        "ByOrganization" -> "CANCELLED_BY_ORGANIZATION"
        "ByAllocator" -> "CANCELLED_BY_ALLOCATOR"
        _ -> error "CancellationReason parsing error"
    }

instance ToSchema CancellationSource where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions cancellationSourceJSONOptions
