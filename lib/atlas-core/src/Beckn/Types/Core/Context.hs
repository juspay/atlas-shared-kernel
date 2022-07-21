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

Module      :  Beckn.Types.Core.Context
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Context (module Beckn.Types.Core.Context, module Reexport) where

import Beckn.Types.App
import Beckn.Types.Core.Domain as Reexport
import Beckn.Utils.Example
import Beckn.Utils.GenericPretty
import Beckn.Utils.JSON
import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import EulerHS.Prelude
import Servant.Client (parseBaseUrl)

data Context = Context
  { domain :: Domain,
    country :: Text,
    city :: Text,
    action :: Action,
    core_version :: Text,
    bap_id :: Text,
    bap_uri :: BaseUrl,
    bpp_id :: Maybe Text,
    bpp_uri :: Maybe BaseUrl,
    transaction_id :: Maybe Text,
    message_id :: Text,
    timestamp :: UTCTime
  }
  deriving (Generic, FromJSON, Show, ToSchema, PrettyShow)

instance ToJSON Context where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}

instance Example Context where
  example =
    Context
      { domain = example,
        action = example,
        core_version = "0.9.3",
        bap_id = "API.DOMAIN",
        bap_uri = fromJust $ parseBaseUrl "https://api.domain.com/",
        bpp_id = Just "API.DOMAIN",
        bpp_uri = parseBaseUrl "https://api.domain.com/",
        transaction_id = Just idExample,
        message_id = idExample,
        timestamp = example,
        country = "IND",
        city = "Kochi"
      }

data Action
  = SEARCH
  | SELECT
  | INIT
  | CONFIRM
  | UPDATE
  | STATUS
  | TRACK
  | CANCEL
  | RATING
  | SUPPORT
  | ON_SEARCH
  | ON_SELECT
  | ON_INIT
  | ON_CONFIRM
  | ON_UPDATE
  | ON_STATUS
  | ON_TRACK
  | ON_CANCEL
  | ON_RATING
  | ON_SUPPORT
  deriving (Generic, Show, Eq, ToSchema)
  deriving (PrettyShow) via Showable Action

instance FromJSON Action where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON Action where
  toJSON = genericToJSON constructorsToLowerOptions

instance Example Action where
  example = SEARCH

mapToCbAction :: Action -> Maybe Action
mapToCbAction = \case
  SEARCH -> Just ON_SEARCH
  SELECT -> Just ON_SELECT
  INIT -> Just ON_INIT
  CONFIRM -> Just ON_CONFIRM
  UPDATE -> Just ON_UPDATE
  STATUS -> Just ON_STATUS
  TRACK -> Just ON_TRACK
  CANCEL -> Just ON_CANCEL
  RATING -> Just ON_RATING
  SUPPORT -> Just ON_SUPPORT
  _ -> Nothing
