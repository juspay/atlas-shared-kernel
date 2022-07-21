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

Module      :  Beckn.Types.Core.Migration.Support
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.Support where

import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Utils.JSON (constructorsToLowerOptions, stripPrefixUnderscoreIfAny)
import EulerHS.Prelude

data Support = Support
  { _type :: Maybe SupportType,
    ref_id :: Maybe Text,
    channels :: Maybe Tags
  }
  deriving (Generic, Show)

instance FromJSON Support where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Support where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data SupportType
  = ORDER
  | BILLING
  | FULFILLMENT
  deriving (Generic, Show, Eq)

instance FromJSON SupportType where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON SupportType where
  toJSON = genericToJSON constructorsToLowerOptions
