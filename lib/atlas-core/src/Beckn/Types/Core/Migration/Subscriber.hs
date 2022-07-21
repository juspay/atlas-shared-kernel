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

Module      :  Beckn.Types.Core.Migration.Subscriber
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.Subscriber where

import Beckn.Types.Core.Domain
import Beckn.Utils.JSON
import Data.Time
import EulerHS.Prelude

data Subscriber = Subscriber
  { subscriber_id :: Maybe Text,
    _type :: Maybe SubscriberType,
    cb_url :: Maybe Text,
    domain :: Maybe Domain,
    city :: Maybe Text,
    country :: Maybe Text,
    signing_public_key :: Maybe Text,
    encryption_public_key :: Maybe Text,
    status :: Maybe SubscriberStatus,
    created :: Maybe UTCTime,
    updated :: Maybe UTCTime,
    expires :: Maybe UTCTime
  }
  deriving (Generic, Show)

data SubscriberType
  = BAP
  | BPP
  | BG
  | BPPR
  | BGR
  deriving (Generic, Show, Eq)

data SubscriberStatus
  = INITIATED
  | UNDER_SUBSCRIPTION
  | SUBSCRIBED
  | INVALID_SSL
  | UNSUBSCRIBED
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance FromJSON SubscriberType where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON SubscriberType where
  toJSON = genericToJSON constructorsToLowerOptions

instance FromJSON Subscriber where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Subscriber where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
