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

Module      :  Beckn.Types.Core.Migration.Domain
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.Domain (Domain (..)) where

import Beckn.Utils.Example
import Beckn.Utils.GenericPretty
import Beckn.Utils.JSON (constructorsWithHyphensUntagged)
import qualified Control.Lens as L
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
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
  | UNKNOWN_DOMAIN Text
  deriving (Eq, Generic, Show)
  deriving (PrettyShow) via Showable Domain

instance ToSchema Domain where
  declareNamedSchema _ = do
    return $
      NamedSchema (Just "Domain") $
        mempty
          & type_ L.?~ OpenApiString
          & enum_
            L.?~ [ "MOBILITY",
                   "LOCAL_RETAIL",
                   "FOOD_AND_BEVERAGE",
                   "HEALTHCARE",
                   "METRO",
                   "PARKING",
                   "PUBLIC_TRANSPORT",
                   "LOGISTICS"
                 ]

instance ToJSON Domain where
  toJSON MOBILITY = String "nic2004:60221"
  toJSON LOCAL_RETAIL = String "nic2004:52110"
  toJSON METRO = String "nic2004:60212"
  toJSON PARKING = String "nic2004:63031"
  toJSON PUBLIC_TRANSPORT = String "nic2004:63032"
  toJSON LOGISTICS = String "nic2004:60232"
  toJSON (UNKNOWN_DOMAIN domain) = String domain
  toJSON val = genericToJSON constructorsWithHyphensUntagged val -- TODO: update remaining domains with codes

instance FromJSON Domain where
  parseJSON (String "nic2004:60221") = pure MOBILITY
  parseJSON (String "nic2004:52110") = pure LOCAL_RETAIL
  parseJSON (String "FOOD-AND-BEVERAGE") = pure FOOD_AND_BEVERAGE
  parseJSON (String "HEALTHCARE") = pure HEALTHCARE
  parseJSON (String "nic2004:60212") = pure METRO
  parseJSON (String "nic2004:63031") = pure PARKING
  parseJSON (String "nic2004:63032") = pure PUBLIC_TRANSPORT
  parseJSON (String "nic2004:60232") = pure LOGISTICS
  parseJSON (String domain) = pure $ UNKNOWN_DOMAIN domain
  parseJSON e = typeMismatch "Core Domain" e

instance Example Domain where
  example = MOBILITY
