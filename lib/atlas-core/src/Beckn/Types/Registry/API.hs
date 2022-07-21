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

Module      :  Beckn.Types.Registry.API
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Registry.API where

import Beckn.Types.Registry.City (City)
import Beckn.Types.Registry.Country (Country)
import Beckn.Types.Registry.Domain (Domain)
import Beckn.Types.Registry.Subscriber (Subscriber, SubscriberType)
import Beckn.Utils.JSON (stripPrefixUnderscoreIfAny)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data LookupRequest = LookupRequest
  { unique_key_id :: Maybe Text,
    subscriber_id :: Maybe Text,
    _type :: Maybe SubscriberType,
    domain :: Maybe Domain,
    country :: Maybe Country,
    city :: Maybe City
  }
  deriving (Show, Generic, ToSchema)

emptyLookupRequest :: LookupRequest
emptyLookupRequest = LookupRequest Nothing Nothing Nothing Nothing Nothing Nothing

instance FromJSON LookupRequest where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON LookupRequest where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

type LookupResponse = [Subscriber]
