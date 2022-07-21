{-# LANGUAGE DerivingVia #-}


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

Module      :  Beckn.Types.Core.Domain
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Domain (Domain (..)) where

import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Example
import Beckn.Utils.GenericPretty
import Beckn.Utils.JSON (replaceUnderscoresString)
import Data.Aeson
import Data.OpenApi hiding (Example)
import EulerHS.Prelude

data Domain
  = MOBILITY
  | LOCAL_RETAIL
  | FOOD_AND_BEVERAGE
  | HEALTHCARE
  | METRO
  | PARKING
  | PUBLIC_TRANSPORT
  | LOGISTICS
  deriving (Eq, Generic, Show, Read, FromDhall)
  deriving (PrettyShow) via Showable Domain

instance Example Domain where
  example = MOBILITY

customAesonOptions :: Options
customAesonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "MOBILITY" -> "nic2004:60221"
        "LOCAL_RETAIL" -> "nic2004:52110"
        "METRO" -> "nic2004:60212"
        "PARKING" -> "nic2004:63031"
        "PUBLIC_TRANSPORT" -> "nic2004:63032"
        "LOGISTICS" -> "nic2004:60232"
        val -> replaceUnderscoresString val, -- TODO: update remaining domains with codes
      sumEncoding = UntaggedValue
    }

instance ToSchema Domain where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions customAesonOptions

instance ToJSON Domain where
  toJSON = genericToJSON customAesonOptions

instance FromJSON Domain where
  parseJSON = genericParseJSON customAesonOptions
